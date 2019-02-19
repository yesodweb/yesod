{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleContexts  #-}
module Yesod.Core.Internal.Run where


import           RIO
import Yesod.Core.Internal.Response
import           Data.ByteString.Builder      (toLazyByteString)
import qualified Data.ByteString.Lazy         as BL
import           Control.Monad.Trans.Resource (runResourceT, withInternalState, runInternalState, InternalState)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Char8        as S8
import qualified Data.Map                     as Map
import           Data.Maybe                   (isJust, fromMaybe)
import           Data.Monoid                  (appEndo)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error     (lenientDecode)
import qualified Network.HTTP.Types           as H
import           Network.Wai
import           Network.Wai.Internal
import           Yesod.Core.Content
import           Yesod.Core.Class.Yesod
import           Yesod.Core.Types
import           Yesod.Core.Internal.Request  (parseWaiRequest,
                                               tooLargeResponse)
import           Yesod.Core.Internal.Util     (getCurrentMaxExpiresRFC1123)
import           Yesod.Routes.Class           (Route, renderRoute)
import           Control.DeepSeq              (($!!), NFData)

-- | Convert a synchronous exception into an ErrorResponse
toErrorHandler :: SomeException -> IO ErrorResponse
toErrorHandler e0 = handleAny errFromShow $
    case fromException e0 of
        Just (HCError x) -> evaluate $!! x
        _ -> errFromShow e0

-- | Generate an @ErrorResponse@ based on the shown version of the exception
errFromShow :: SomeException -> IO ErrorResponse
errFromShow x = do
  text <- evaluate (T.pack $ show x) `catchAny` \_ ->
          return (T.pack "Yesod.Core.Internal.Run.errFromShow: show of an exception threw an exception")
  return $ InternalError text

-- | Do a basic run of a handler, getting some contents and the final
-- @GHState@. The @GHState@ unfortunately may contain some impure
-- exceptions, but all other synchronous exceptions will be caught and
-- represented by the @HandlerContents@.
basicRunHandler :: ToTypedContent c
                => RunHandlerEnv site site
                -> HandlerFor site c
                -> YesodRequest
                -> InternalState
                -> IO (GHState, HandlerContents)
basicRunHandler rhe handler yreq resState = do
    -- Create a mutable ref to hold the state. We use mutable refs so
    -- that the updates will survive runtime exceptions.
    istate <- newIORef defState

    -- Run the handler itself, capturing any runtime exceptions and
    -- converting them into a @HandlerContents@
    contents' <- catchAny
        (do
            res <- runRIO (hd istate) handler
            tc <- evaluate (toTypedContent res)
            -- Success! Wrap it up in an @HCContent@
            return (HCContent defaultStatus tc))
        (\e ->
            case fromException e of
                Just e' -> return e'
                Nothing -> HCError <$> toErrorHandler e)

    -- Get the raw state and return
    state <- readIORef istate
    return (state, contents')
  where
    defState = GHState
        { ghsSession = reqSession yreq
        , ghsRBC = Nothing
        , ghsIdent = 1
        , ghsCache = mempty
        , ghsCacheBy = mempty
        , ghsHeaders = mempty
        }
    hd istate = HandlerData $ SubHandlerData
        { handlerRequest = yreq
        , handlerEnv     = rhe
        , handlerState   = istate
        , handlerResource = resState
        }

-- | Convert an @ErrorResponse@ into a @YesodResponse@
handleError :: RunHandlerEnv sub site
            -> YesodRequest
            -> InternalState
            -> Map.Map Text S8.ByteString
            -> [Header]
            -> ErrorResponse
            -> IO YesodResponse
handleError rhe yreq resState finalSession headers e0 = do
    -- Find any evil hidden impure exceptions
    e <- (evaluate $!! e0) `catchAny` errFromShow

    -- Generate a response, leveraging the updated session and
    -- response headers
    flip runInternalState resState $ do
        yar <- rheOnError rhe e yreq
            { reqSession = finalSession
            }
        case yar of
            YRPlain status' hs ct c sess ->
                let hs' = headers ++ hs
                    status
                        | status' == defaultStatus = getStatus e
                        | otherwise = status'
                in return $ YRPlain status hs' ct c sess
            YRWai _ -> return yar
            YRWaiApp _ -> return yar

-- | Convert a @HandlerContents@ into a @YesodResponse@
handleContents :: (ErrorResponse -> IO YesodResponse)
               -> Map.Map Text S8.ByteString
               -> [Header]
               -> HandlerContents
               -> IO YesodResponse
handleContents handleError' finalSession headers contents =
    case contents of
        HCContent status (TypedContent ct c) -> do
            -- Check for impure exceptions hiding in the contents
            ec' <- evaluateContent c
            case ec' of
                Left e -> handleError' e
                Right c' -> return $ YRPlain status headers ct c' finalSession
        HCError e -> handleError' e
        HCRedirect status loc -> do
            let disable_caching x =
                      Header "Cache-Control" "no-cache, must-revalidate"
                    : Header "Expires" "Thu, 01 Jan 1970 05:05:05 GMT"
                    : x
                hs = (if status /= H.movedPermanently301 then disable_caching else id)
                      $ Header "Location" (encodeUtf8 loc) : headers
            return $ YRPlain
                status hs typePlain emptyContent
                finalSession
        HCSendFile ct fp p -> return $ YRPlain
            H.status200
            headers
            ct
            (ContentFile fp p)
            finalSession
        HCCreated loc -> return $ YRPlain
            H.status201
            (Header "Location" (encodeUtf8 loc) : headers)
            typePlain
            emptyContent
            finalSession
        HCWai r -> return $ YRWai r
        HCWaiApp a -> return $ YRWaiApp a

-- | Evaluate the given value. If an exception is thrown, use it to
-- replace the provided contents and then return @mempty@ in place of the
-- evaluated value.
evalFallback :: (Monoid w, NFData w)
             => HandlerContents
             -> w
             -> IO (w, HandlerContents)
evalFallback contents val = catchAny
    (fmap (, contents) (evaluate $!! val))
    (fmap ((mempty, ) . HCError) . toErrorHandler)

-- | Function used internally by Yesod in the process of converting a
-- 'HandlerT' into an 'Application'. Should not be needed by users.
runHandler :: ToTypedContent c
           => RunHandlerEnv site site
           -> HandlerFor site c
           -> YesodApp
runHandler rhe@RunHandlerEnv {..} handler yreq = withInternalState $ \resState -> do
    -- Get the raw state and original contents
    (state, contents0) <- basicRunHandler rhe handler yreq resState

    -- Evaluate the unfortunately-lazy session and headers,
    -- propagating exceptions into the contents
    (finalSession, contents1) <- evalFallback contents0 (ghsSession state)
    (headers, contents2) <- evalFallback contents1 (appEndo (ghsHeaders state) [])
    contents3 <- (evaluate contents2) `catchAny` (fmap HCError . toErrorHandler)

    -- Convert the HandlerContents into the final YesodResponse
    handleContents
        (handleError rhe yreq resState finalSession headers)
        finalSession
        headers
        contents3

safeEh :: LogFunc -> ErrorResponse -> YesodApp
safeEh logFunc er req = do
    runRIO logFunc $
      logErrorS "yesod-core" $
      "Error handler errored out: " <> displayShow er
    return $ YRPlain
        H.status500
        []
        typePlain
        (toContent ("Internal Server Error" :: S.ByteString))
        (reqSession req)

-- | Run a 'HandlerT' completely outside of Yesod.  This
-- function comes with many caveats and you shouldn't use it
-- unless you fully understand what it's doing and how it works.
--
-- As of now, there's only one reason to use this function at
-- all: in order to run unit tests of functions inside 'HandlerT'
-- but that aren't easily testable with a full HTTP request.
-- Even so, it's better to use @wai-test@ or @yesod-test@ instead
-- of using this function.
--
-- This function will create a fake HTTP request (both @wai@'s
-- 'Request' and @yesod@'s 'Request') and feed it to the
-- @HandlerT@.  The only useful information the @HandlerT@ may
-- get from the request is the session map, which you must supply
-- as argument to @runFakeHandler@.  All other fields contain
-- fake information, which means that they can be accessed but
-- won't have any useful information.  The response of the
-- @HandlerT@ is completely ignored, including changes to the
-- session, cookies or headers.  We only return you the
-- @HandlerT@'s return value.
runFakeHandler :: (Yesod site, MonadIO m) =>
                  SessionMap
               -> LogFunc
               -> site
               -> HandlerFor site a
               -> m (Either ErrorResponse a)
runFakeHandler fakeSessionMap logFunc site handler = liftIO $ do
  ret <- newIORef (Left $ InternalError "runFakeHandler: no result")
  maxExpires <- getCurrentMaxExpiresRFC1123
  let handler' = writeIORef ret . Right =<< handler
  let yapp = runHandler
         RunHandlerEnv
            { rheRender = yesodRender site $ resolveApproot site fakeWaiRequest
            , rheRoute = Nothing
            , rheRouteToMaster = id
            , rheChild = site
            , rheSite = site
            , rheUpload = fileUpload site
            , rheLogFunc = logFunc
            , rheOnError = errHandler
            , rheMaxExpires = maxExpires
            }
        handler'
      errHandler err req = do
          writeIORef ret (Left err)
          return $ YRPlain
                     H.status500
                     []
                     typePlain
                     (toContent ("runFakeHandler: errHandler" :: S8.ByteString))
                     (reqSession req)
      fakeWaiRequest = Request
          { requestMethod  = "POST"
          , httpVersion    = H.http11
          , rawPathInfo    = "/runFakeHandler/pathInfo"
          , rawQueryString = ""
          , requestHeaderHost = Nothing
          , requestHeaders = []
          , isSecure       = False
          , remoteHost     = error "runFakeHandler-remoteHost"
          , pathInfo       = ["runFakeHandler", "pathInfo"]
          , queryString    = []
          , requestBody    = return mempty
          , vault          = mempty
          , requestBodyLength = KnownLength 0
          , requestHeaderRange = Nothing
          , requestHeaderReferer = Nothing
          , requestHeaderUserAgent = Nothing
          }
      fakeRequest =
        YesodRequest
          { reqGetParams  = []
          , reqCookies    = []
          , reqWaiRequest = fakeWaiRequest
          , reqLangs      = []
          , reqToken      = Just "NaN" -- not a nonce =)
          , reqAccept     = []
          , reqSession    = fakeSessionMap
          }
  _ <- runResourceT $ yapp fakeRequest
  readIORef ret

yesodRunner :: (ToTypedContent res, Yesod site)
            => HandlerFor site res
            -> YesodRunnerEnv site
            -> Maybe (Route site)
            -> Application
yesodRunner handler' YesodRunnerEnv {..} route req sendResponse
  | Just maxLen <- mmaxLen, KnownLength len <- requestBodyLength req, maxLen < len = sendResponse (tooLargeResponse maxLen len)
  | otherwise = do
    let dontSaveSession _ = return []
    (session, saveSession) <- liftIO $
        maybe (return (Map.empty, dontSaveSession)) (`sbLoadSession` req) yreSessionBackend
    maxExpires <- yreGetMaxExpires
    let mkYesodReq = parseWaiRequest req session (isJust yreSessionBackend) mmaxLen
    let yreq =
            case mkYesodReq of
                Left yreq' -> yreq'
                Right needGen -> needGen yreGen
    let ra = resolveApproot yreSite req
    let -- We set up two environments: the first one has a "safe" error handler
        -- which will never throw an exception. The second one uses the
        -- user-provided errorHandler function. If that errorHandler function
        -- errors out, it will use the safeEh below to recover.
        rheSafe = RunHandlerEnv
            { rheRender = yesodRender yreSite ra
            , rheRoute = route
            , rheRouteToMaster = id
            , rheChild = yreSite
            , rheSite = yreSite
            , rheUpload = fileUpload yreSite
            , rheLogFunc = yreLogFunc
            , rheOnError = safeEh yreLogFunc
            , rheMaxExpires = maxExpires
            }
        rhe = rheSafe
            { rheOnError = runHandler rheSafe . errorHandler
            }

    yesodWithInternalState yreSite route $ \is -> do
        yreq' <- yreq
        yar <- runInternalState (runHandler rhe handler yreq') is
        yarToResponse yar saveSession yreq' req is sendResponse
  where
    mmaxLen = maximumContentLength yreSite route
    handler = yesodMiddleware handler'

yesodRender :: Yesod y
            => y
            -> ResolvedApproot
            -> Route y
            -> [(Text, Text)] -- ^ url query string
            -> Text
yesodRender y ar url params =
    decodeUtf8With lenientDecode $ BL.toStrict $ toLazyByteString $
    fromMaybe
        (joinPath y ar ps
          $ params ++ params')
        (urlParamRenderOverride y url params)
  where
    (ps, params') = renderRoute url

resolveApproot :: Yesod master => master -> Request -> ResolvedApproot
resolveApproot master req =
    case approot of
        ApprootRelative -> ""
        ApprootStatic t -> t
        ApprootMaster f -> f master
        ApprootRequest f -> f master req
