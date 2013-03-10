{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Core.Run where

import           Blaze.ByteString.Builder     (fromLazyByteString, toByteString,
                                               toLazyByteString)
import           Control.Applicative          ((<$>))
import           Control.Exception            (SomeException, fromException,
                                               handle)
import           Control.Exception.Lifted     (catch)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Char8        as S8
import qualified Data.ByteString.Lazy         as L
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
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
import qualified Network.HTTP.Types           as H
import           Network.Wai
import           Prelude                      hiding (catch)
import           System.Log.FastLogger        (Logger)
import           System.Random                (newStdGen)
import           Web.Cookie                   (renderSetCookie)
import           Yesod.Content
import           Yesod.Core.Class
import           Yesod.Core.Types
import           Yesod.Internal               (tokenKey)
import           Yesod.Internal.Request       (parseWaiRequest,
                                               tooLargeResponse)
import           Yesod.Routes.Class           (Route, renderRoute)
import Language.Haskell.TH.Syntax (Loc, qLocation)
import Control.Monad.Logger (LogSource, LogLevel (LevelError), liftLoc)
import System.Log.FastLogger (LogStr, toLogStr)

yarToResponse :: YesodResponse -> [(CI ByteString, ByteString)] -> Response
yarToResponse (YRWai a) _ = a
yarToResponse (YRPlain s hs _ c _) extraHeaders =
    go c
  where
    finalHeaders = extraHeaders ++ map headerToPair hs
    finalHeaders' len = ("Content-Length", S8.pack $ show len)
                      : finalHeaders

    go (ContentBuilder b mlen) =
        ResponseBuilder s hs' b
      where
        hs' = maybe finalHeaders finalHeaders' mlen
    go (ContentFile fp p) = ResponseFile s finalHeaders fp p
    go (ContentSource body) = ResponseSource s finalHeaders body
    go (ContentDontEvaluate c') = go c'

-- | Convert Header to a key/value pair.
headerToPair :: Header
             -> (CI ByteString, ByteString)
headerToPair (AddCookie sc) =
    ("Set-Cookie", toByteString $ renderSetCookie $ sc)
headerToPair (DeleteCookie key path) =
    ( "Set-Cookie"
    , S.concat
        [ key
        , "=; path="
        , path
        , "; expires=Thu, 01-Jan-1970 00:00:00 GMT"
        ]
    )
headerToPair (Header key value) = (CI.mk key, value)

localNoCurrent :: GHandler s m a -> GHandler s m a
localNoCurrent =
    local (\hd -> hd { handlerEnv = (handlerEnv hd) { rheRoute = Nothing }})

local :: (HandlerData sub' master' -> HandlerData sub master)
      -> GHandler sub master a
      -> GHandler sub' master' a
local f (GHandler x) = GHandler $ \r -> x $ f r

-- | Function used internally by Yesod in the process of converting a
-- 'GHandler' into an 'Application'. Should not be needed by users.
runHandler :: HasReps c
           => RunHandlerEnv sub master
           -> GHandler sub master c
           -> YesodApp
runHandler rhe@RunHandlerEnv {..} handler yreq = do
    let toErrorHandler e =
            case fromException e of
                Just (HCError x) -> x
                _ -> InternalError $ T.pack $ show e
    istate <- liftIO $ I.newIORef GHState
        { ghsSession = initSession
        , ghsRBC = Nothing
        , ghsIdent = 1
        , ghsCache = mempty
        , ghsHeaders = mempty
        }
    let hd = HandlerData
            { handlerRequest = yreq
            , handlerEnv     = rhe
            , handlerState   = istate
            }
    contents' <- catch (fmap Right $ unGHandler handler hd)
        (\e -> return $ Left $ maybe (HCError $ toErrorHandler e) id
                      $ fromException e)
    state <- liftIO $ I.readIORef istate
    let finalSession = ghsSession state
    let headers = ghsHeaders state
    let contents = either id (HCContent H.status200 . chooseRep) contents'
    let handleError e = do
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
        HCContent status a -> do
            (ct, c) <- liftIO $ a cts
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
  where
    cts = reqAccept yreq
    initSession = reqSession yreq

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

evaluateContent :: Content -> IO (Either ErrorResponse Content)
evaluateContent (ContentBuilder b mlen) = Control.Exception.handle f $ do
    let lbs = toLazyByteString b
    L.length lbs `seq` return (Right $ ContentBuilder (fromLazyByteString lbs) mlen)
  where
    f :: SomeException -> IO (Either ErrorResponse Content)
    f = return . Left . InternalError . T.pack . show
evaluateContent c = return (Right c)

getStatus :: ErrorResponse -> H.Status
getStatus NotFound = H.status404
getStatus (InternalError _) = H.status500
getStatus (InvalidArgs _) = H.status400
getStatus (PermissionDenied _) = H.status403
getStatus (BadMethod _) = H.status405

-- | Run a 'GHandler' completely outside of Yesod.  This
-- function comes with many caveats and you shouldn't use it
-- unless you fully understand what it's doing and how it works.
--
-- As of now, there's only one reason to use this function at
-- all: in order to run unit tests of functions inside 'GHandler'
-- but that aren't easily testable with a full HTTP request.
-- Even so, it's better to use @wai-test@ or @yesod-test@ instead
-- of using this function.
--
-- This function will create a fake HTTP request (both @wai@'s
-- 'Request' and @yesod@'s 'Request') and feed it to the
-- @GHandler@.  The only useful information the @GHandler@ may
-- get from the request is the session map, which you must supply
-- as argument to @runFakeHandler@.  All other fields contain
-- fake information, which means that they can be accessed but
-- won't have any useful information.  The response of the
-- @GHandler@ is completely ignored, including changes to the
-- session, cookies or headers.  We only return you the
-- @GHandler@'s return value.
runFakeHandler :: (Yesod master, MonadIO m) =>
                  SessionMap
               -> (master -> Logger)
               -> master
               -> GHandler master master a
               -> m (Either ErrorResponse a)
runFakeHandler fakeSessionMap logger master handler = liftIO $ do
  ret <- I.newIORef (Left $ InternalError "runFakeHandler: no result")
  let handler' = do liftIO . I.writeIORef ret . Right =<< handler
                    return ()
  let yapp = runHandler
         RunHandlerEnv
            { rheRender = yesodRender master $ resolveApproot master fakeWaiRequest
            , rheRoute = Nothing
            , rheToMaster = id
            , rheMaster = master
            , rheSub = master
            , rheUpload = fileUpload master
            , rheLog = messageLoggerSource master $ logger master
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

defaultYesodRunner :: Yesod master
                   => YesodRunnerEnv sub master
                   -> GHandler sub master ChooseRep
                   -> Application
defaultYesodRunner YesodRunnerEnv {..} handler' req
  | KnownLength len <- requestBodyLength req, maxLen < len = return tooLargeResponse
  | otherwise = do
    let dontSaveSession _ = return []
    (session, saveSession) <- liftIO $ do
        maybe (return (Map.empty, dontSaveSession)) (\sb -> sbLoadSession sb yreMaster req) yreSessionBackend
    let mkYesodReq = parseWaiRequest req session (isJust yreSessionBackend) maxLen
    yreq <-
        case mkYesodReq of
            Left yreq -> return yreq
            Right needGen -> liftIO $ needGen <$> newStdGen
    let ra = resolveApproot yreMaster req
    let log' = messageLoggerSource yreMaster yreLogger
        -- We set up two environments: the first one has a "safe" error handler
        -- which will never throw an exception. The second one uses the
        -- user-provided errorHandler function. If that errorHandler function
        -- errors out, it will use the safeEh below to recover.
        rheSafe = RunHandlerEnv
            { rheRender = yesodRender yreMaster ra
            , rheRoute = yreRoute
            , rheToMaster = yreToMaster
            , rheMaster = yreMaster
            , rheSub = yreSub
            , rheUpload = fileUpload yreMaster
            , rheLog = log'
            , rheOnError = safeEh log'
            }
        rhe = rheSafe
            { rheOnError = runHandler rheSafe . localNoCurrent . errorHandler
            }
    yar <- runHandler rhe handler yreq
    extraHeaders <- case yar of
        (YRPlain _ _ ct _ newSess) -> do
            let nsToken = maybe
                    newSess
                    (\n -> Map.insert tokenKey (encodeUtf8 n) newSess)
                    (reqToken yreq)
            sessionHeaders <- liftIO (saveSession nsToken)
            return $ ("Content-Type", ct) : map headerToPair sessionHeaders
        _ -> return []
    return $ yarToResponse yar extraHeaders
  where
    maxLen = maximumContentLength yreMaster $ fmap yreToMaster yreRoute
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

toMasterHandlerMaybe :: (Route sub -> Route master)
                     -> (master -> sub)
                     -> Maybe (Route sub)
                     -> GHandler sub master a
                     -> GHandler sub' master a
toMasterHandlerMaybe tm ts route = local (handlerSubDataMaybe tm ts route)

-- | FIXME do we need this?
toMasterHandlerDyn :: (Route sub -> Route master)
                   -> GHandler sub' master sub
                   -> Route sub
                   -> GHandler sub master a
                   -> GHandler sub' master a
toMasterHandlerDyn tm getSub route h = do
    sub <- getSub
    local (handlerSubData tm (const sub) route) h

-- | Used internally for promoting subsite handler functions to master site
-- handler functions. Should not be needed by users.
toMasterHandler :: (Route sub -> Route master)
                -> (master -> sub)
                -> Route sub
                -> GHandler sub master a
                -> GHandler sub' master a
toMasterHandler tm ts route = local (handlerSubData tm ts route)

handlerSubData :: (Route sub -> Route master)
               -> (master -> sub)
               -> Route sub
               -> HandlerData oldSub master
               -> HandlerData sub master
handlerSubData tm ts = handlerSubDataMaybe tm ts . Just

handlerSubDataMaybe :: (Route sub -> Route master)
                    -> (master -> sub)
                    -> Maybe (Route sub)
                    -> HandlerData oldSub master
                    -> HandlerData sub master
handlerSubDataMaybe tm ts route hd = hd
    { handlerEnv = (handlerEnv hd)
        { rheSub      = ts $ rheMaster $ handlerEnv hd
        , rheToMaster = tm
        , rheRoute    = route
        }
    }

resolveApproot :: Yesod master => master -> Request -> ResolvedApproot
resolveApproot master req =
    case approot of
        ApprootRelative -> ""
        ApprootStatic t -> t
        ApprootMaster f -> f master
        ApprootRequest f -> f master req
