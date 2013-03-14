{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
module Yesod.Core.Internal.Run where

import Yesod.Core.Internal.Response
import           Blaze.ByteString.Builder     (toByteString)
import           Control.Applicative          ((<$>))
import           Control.Exception            (fromException)
import           Control.Exception.Lifted     (catch)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (LogLevel (LevelError), LogSource,
                                               liftLoc)
import           Control.Monad.Trans.Resource (runResourceT, withInternalState, runInternalState)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Char8        as S8
import qualified Data.IORef                   as I
import qualified Data.Map                     as Map
import           Data.Maybe                   (isJust)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  (appEndo, mempty)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8)
import           Data.Text.Encoding           (decodeUtf8With)
import           Data.Text.Encoding.Error     (lenientDecode)
import           Language.Haskell.TH.Syntax   (Loc, qLocation)
import qualified Network.HTTP.Types           as H
import           Network.Wai
import           Prelude                      hiding (catch)
import           System.Log.FastLogger        (Logger)
import           System.Log.FastLogger        (LogStr, toLogStr)
import           System.Random                (newStdGen)
import           Yesod.Core.Content
import           Yesod.Core.Class.Yesod
import           Yesod.Core.Types
import           Yesod.Core.Internal.Request  (parseWaiRequest,
                                               tooLargeResponse)
import           Yesod.Routes.Class           (Route, renderRoute)

-- | Function used internally by Yesod in the process of converting a
-- 'HandlerT' into an 'Application'. Should not be needed by users.
runHandler :: ToTypedContent c
           => RunHandlerEnv site
           -> HandlerT site IO c
           -> YesodApp
runHandler rhe@RunHandlerEnv {..} handler yreq = withInternalState $ \resState -> do
    let toErrorHandler e =
            case fromException e of
                Just (HCError x) -> x
                _ -> InternalError $ T.pack $ show e
    istate <- liftIO $ I.newIORef GHState
        { ghsSession = reqSession yreq
        , ghsRBC = Nothing
        , ghsIdent = 1
        , ghsCache = mempty
        , ghsHeaders = mempty
        }
    let hd = HandlerData
            { handlerRequest = yreq
            , handlerEnv     = rhe
            , handlerState   = istate
            , handlerToParent = const ()
            , handlerResource = resState
            }
    contents' <- catch (fmap Right $ unHandlerT handler hd)
        (\e -> return $ Left $ maybe (HCError $ toErrorHandler e) id
                      $ fromException e)
    state <- liftIO $ I.readIORef istate
    let finalSession = ghsSession state
    let headers = ghsHeaders state
    let contents = either id (HCContent H.status200 . toTypedContent) contents'
    let handleError e = flip runInternalState resState $ do
            yar <- rheOnError e yreq
                { reqSession = finalSession
                }
            case yar of
                YRPlain _ hs ct c sess ->
                    let hs' = appEndo headers hs
                     in return $ YRPlain (getStatus e) hs' ct c sess
                YRWai _ -> return yar
    let sendFile' ct fp p =
            return $ YRPlain H.status200 (appEndo headers []) ct (ContentFile fp p) finalSession
    case contents of
        HCContent status (TypedContent ct c) -> do
            ec' <- liftIO $ evaluateContent c
            case ec' of
                Left e -> handleError e
                Right c' -> return $ YRPlain status (appEndo headers []) ct c' finalSession
        HCError e -> handleError e
        HCRedirect status loc -> do
            let disable_caching x =
                      Header "Cache-Control" "no-cache, must-revalidate"
                    : Header "Expires" "Thu, 01 Jan 1970 05:05:05 GMT"
                    : x
                hs = (if status /= H.movedPermanently301 then disable_caching else id)
                      $ Header "Location" (encodeUtf8 loc) : appEndo headers []
            return $ YRPlain
                status hs typePlain emptyContent
                finalSession
        HCSendFile ct fp p -> catch
            (sendFile' ct fp p)
            (handleError . toErrorHandler)
        HCCreated loc -> do
            let hs = Header "Location" (encodeUtf8 loc) : appEndo headers []
            return $ YRPlain
                H.status201
                hs
                typePlain
                emptyContent
                finalSession
        HCWai r -> return $ YRWai r

safeEh :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
       -> ErrorResponse
       -> YesodApp
safeEh log' er req = do
    liftIO $ log' $(qLocation >>= liftLoc) "yesod-core" LevelError
           $ toLogStr $ "Error handler errored out: " ++ show er
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
               -> (site -> Logger)
               -> site
               -> HandlerT site IO a
               -> m (Either ErrorResponse a)
runFakeHandler fakeSessionMap logger site handler = liftIO $ do
  ret <- I.newIORef (Left $ InternalError "runFakeHandler: no result")
  let handler' = do liftIO . I.writeIORef ret . Right =<< handler
                    return ()
  let yapp = runHandler
         RunHandlerEnv
            { rheRender = yesodRender site $ resolveApproot site fakeWaiRequest
            , rheRoute = Nothing
            , rheSite = site
            , rheUpload = fileUpload site
            , rheLog = messageLoggerSource site $ logger site
            , rheOnError = errHandler
            }
        handler'
      errHandler err req = do
          liftIO $ I.writeIORef ret (Left err)
          return $ YRPlain
                     H.status500
                     []
                     typePlain
                     (toContent ("runFakeHandler: errHandler" :: S8.ByteString))
                     (reqSession req)
      fakeWaiRequest =
        Request
          { requestMethod  = "POST"
          , httpVersion    = H.http11
          , rawPathInfo    = "/runFakeHandler/pathInfo"
          , rawQueryString = ""
          , serverName     = "runFakeHandler-serverName"
          , serverPort     = 80
          , requestHeaders = []
          , isSecure       = False
          , remoteHost     = error "runFakeHandler-remoteHost"
          , pathInfo       = ["runFakeHandler", "pathInfo"]
          , queryString    = []
          , requestBody    = mempty
          , vault          = mempty
          , requestBodyLength = KnownLength 0
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
  I.readIORef ret
{-# WARNING runFakeHandler "Usually you should *not* use runFakeHandler unless you really understand how it works and why you need it." #-}

yesodRunner :: (ToTypedContent res, Yesod site)
            => HandlerT site IO res
            -> YesodRunnerEnv site
            -> Maybe (Route site)
            -> Application
yesodRunner handler' YesodRunnerEnv {..} route req
  | KnownLength len <- requestBodyLength req, maxLen < len = return tooLargeResponse
  | otherwise = do
    let dontSaveSession _ = return []
    (session, saveSession) <- liftIO $ do
        maybe (return (Map.empty, dontSaveSession)) (\sb -> sbLoadSession sb req) yreSessionBackend
    let mkYesodReq = parseWaiRequest req session (isJust yreSessionBackend) maxLen
    yreq <-
        case mkYesodReq of
            Left yreq -> return yreq
            Right needGen -> liftIO $ needGen <$> newStdGen
    let ra = resolveApproot yreSite req
    let log' = messageLoggerSource yreSite yreLogger
        -- We set up two environments: the first one has a "safe" error handler
        -- which will never throw an exception. The second one uses the
        -- user-provided errorHandler function. If that errorHandler function
        -- errors out, it will use the safeEh below to recover.
        rheSafe = RunHandlerEnv
            { rheRender = yesodRender yreSite ra
            , rheRoute = route
            , rheSite = yreSite
            , rheUpload = fileUpload yreSite
            , rheLog = log'
            , rheOnError = safeEh log'
            }
        rhe = rheSafe
            { rheOnError = runHandler rheSafe . errorHandler
            }
    yar <- runHandler rhe handler yreq
    liftIO $ yarToResponse yar saveSession yreq
  where
    maxLen = maximumContentLength yreSite route
    handler = yesodMiddleware handler'

yesodRender :: Yesod y
            => y
            -> ResolvedApproot
            -> Route y
            -> [(Text, Text)] -- ^ url query string
            -> Text
yesodRender y ar url params =
    decodeUtf8With lenientDecode $ toByteString $
    fromMaybe
        (joinPath y ar ps
          $ params ++ params')
        (urlRenderOverride y url)
  where
    (ps, params') = renderRoute url

resolveApproot :: Yesod master => master -> Request -> ResolvedApproot
resolveApproot master req =
    case approot of
        ApprootRelative -> ""
        ApprootStatic t -> t
        ApprootMaster f -> f master
        ApprootRequest f -> f master req

stripHandlerT :: HandlerT child (HandlerT parent m) a
              -> (parent -> child)
              -> (Route child -> Route parent)
              -> Maybe (Route child)
              -> HandlerT parent m a
stripHandlerT (HandlerT f) getSub toMaster newRoute = HandlerT $ \hd -> do
    let env = handlerEnv hd
    ($ hd) $ unHandlerT $ f hd
        { handlerEnv = env
            { rheSite = getSub $ rheSite env
            , rheRoute = newRoute
            , rheRender = \url params -> rheRender env (toMaster url) params
            }
        , handlerToParent = toMaster
        }
