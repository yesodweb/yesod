{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
---------------------------------------------------------
--
-- Module        : Yesod.Handler
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : stable
-- Portability   : portable
--
-- Define Handler stuff.
--
---------------------------------------------------------
module Yesod.Core.Handler
    ( -- * Handler monad
      HandlerT
      -- ** Read information from handler
    , getYesod
    , getsYesod
    , getUrlRender
    , getUrlRenderParams
    , getPostParams
    , getCurrentRoute
    , getRequest
    , waiRequest
    , runRequestBody
    , rawRequestBody
      -- ** Request information
      -- *** Request datatype
    , RequestBodyContents
    , YesodRequest (..)
    , FileInfo
    , fileName
    , fileContentType
    , fileSource
    , fileMove
      -- *** Convenience functions
    , languages
      -- *** Lookup parameters
    , lookupGetParam
    , lookupPostParam
    , lookupCookie
    , lookupFile
    , lookupHeader
      -- **** Lookup authentication data
    , lookupBasicAuth
    , lookupBearerAuth
      -- **** Multi-lookup
    , lookupGetParams
    , lookupPostParams
    , lookupCookies
    , lookupFiles
    , lookupHeaders
      -- * Responses
      -- ** Pure
    , respond
      -- ** Streaming
    , respondSource
    , sendChunk
    , sendFlush
    , sendChunkBS
    , sendChunkLBS
    , sendChunkText
    , sendChunkLazyText
    , sendChunkHtml
      -- ** Redirecting
    , RedirectUrl (..)
    , redirect
    , redirectWith
    , redirectToPost
    , Fragment(..)
      -- ** Errors
    , notFound
    , badMethod
    , notAuthenticated
    , permissionDenied
    , permissionDeniedI
    , invalidArgs
    , invalidArgsI
      -- ** Short-circuit responses.
    , sendFile
    , sendFilePart
    , sendResponse
    , sendResponseStatus
      -- ** Type specific response with custom status
    , sendStatusJSON
    , sendResponseCreated
    , sendWaiResponse
    , sendWaiApplication
    , sendRawResponse
    , sendRawResponseNoConduit
    , notModified
      -- * Different representations
      -- $representations
    , selectRep
    , provideRep
    , provideRepType
    , ProvidedRep
      -- * Setting headers
    , setCookie
    , getExpires
    , deleteCookie
    , addHeader
    , setHeader
    , replaceOrAddHeader
    , setLanguage
      -- ** Content caching and expiration
    , cacheSeconds
    , neverExpires
    , alreadyExpired
    , expiresAt
    , setEtag
    , setWeakEtag
      -- * Session
    , SessionMap
    , lookupSession
    , lookupSessionBS
    , getSession
    , setSession
    , setSessionBS
    , deleteSession
    , clearSession
      -- ** Ultimate destination
    , setUltDest
    , setUltDestCurrent
    , setUltDestReferer
    , redirectUltDest
    , clearUltDest
      -- ** Messages
    , addMessage
    , addMessageI
    , getMessages
    , setMessage
    , setMessageI
    , getMessage
      -- * Helpers for specific content
      -- ** Hamlet
    , hamletToRepHtml
    , giveUrlRenderer
    , withUrlRenderer
      -- ** Misc
    , newIdent
      -- * Lifting
    , handlerToIO
    , forkHandler
      -- * i18n
    , getMessageRender
      -- * Per-request caching
    , cached
    , cachedBy
    , stripHandlerT
      -- * AJAX CSRF protection

      -- $ajaxCSRFOverview

      -- ** Setting CSRF Cookies
    , setCsrfCookie
    , setCsrfCookieWithCookie
    , defaultCsrfCookieName
      -- ** Looking up CSRF Headers
    , checkCsrfHeaderNamed
    , hasValidCsrfHeaderNamed
    , defaultCsrfHeaderName
      -- ** Looking up CSRF POST Parameters
    , hasValidCsrfParamNamed
    , checkCsrfParamNamed
    , defaultCsrfParamName
      -- ** Checking CSRF Headers or POST Parameters
    , checkCsrfHeaderOrParam
    ) where

import           Data.Time                     (UTCTime, addUTCTime,
                                                getCurrentTime)
import           Yesod.Core.Internal.Request   (langKey, mkFileInfoFile,
                                                mkFileInfoLBS, mkFileInfoSource)


#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative           ((<$>))
import           Data.Monoid                   (mempty, mappend)
#endif
import           Control.Applicative           ((<|>))
import           Control.Exception             (evaluate, SomeException, throwIO)
import           Control.Exception.Lifted      (handle)

import           Control.Monad                 (void, liftM, unless)
import qualified Control.Monad.Trans.Writer    as Writer

import           Control.Monad.IO.Class        (MonadIO, liftIO)

import qualified Network.HTTP.Types            as H
import qualified Network.Wai                   as W
import           Network.Wai.Middleware.HttpAuth
    ( extractBasicAuth, extractBearerAuth )
import Control.Monad.Trans.Class (lift)

import           Data.Aeson                    (ToJSON(..))
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8With, encodeUtf8, decodeUtf8)
import           Data.Text.Encoding.Error      (lenientDecode)
import qualified Data.Text.Lazy                as TL
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Hamlet                   (Html, HtmlUrl, hamlet)

import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.Map                      as Map
import qualified Data.HashMap.Strict           as HM

import           Data.Byteable                 (constEqBytes)

import           Control.Arrow                 ((***))
import qualified Data.ByteString.Char8         as S8
import           Data.Monoid                   (Endo (..))
import           Data.Text                     (Text)
import qualified Network.Wai.Parse             as NWP
import           Text.Shakespeare.I18N         (RenderMessage (..))
import           Web.Cookie                    (SetCookie (..))
import           Yesod.Core.Content            (ToTypedContent (..), simpleContentType, contentTypeTypes, HasContentType (..), ToContent (..), ToFlushBuilder (..))
import           Yesod.Core.Internal.Util      (formatRFC1123)
import           Text.Blaze.Html               (preEscapedToHtml, toHtml)

import qualified Data.IORef.Lifted             as I
import           Data.Maybe                    (listToMaybe, mapMaybe)
import           Data.Typeable                 (Typeable)
import           Web.PathPieces                (PathPiece(..))
import           Yesod.Core.Class.Handler
import           Yesod.Core.Types
import           Yesod.Routes.Class            (Route)
import           Blaze.ByteString.Builder (Builder)
import           Safe (headMay)
import           Data.CaseInsensitive (CI, original)
import qualified Data.Conduit.List as CL
import           Control.Monad.Trans.Resource  (MonadResource, InternalState, runResourceT, withInternalState, getInternalState, liftResourceT, resourceForkIO)
import qualified System.PosixCompat.Files as PC
import           Control.Monad.Trans.Control (control, MonadBaseControl)
import           Data.Conduit (Source, transPipe, Flush (Flush), yield, Producer, Sink)
import qualified Yesod.Core.TypeCache as Cache
import qualified Data.Word8 as W8
import qualified Data.Foldable as Fold
import           Data.Default
import           Control.Monad.Logger (MonadLogger, logWarnS)

get :: MonadHandler m => m GHState
get = liftHandlerT $ HandlerT $ I.readIORef . handlerState

put :: MonadHandler m => GHState -> m ()
put x = liftHandlerT $ HandlerT $ flip I.writeIORef x . handlerState

modify :: MonadHandler m => (GHState -> GHState) -> m ()
modify f = liftHandlerT $ HandlerT $ flip I.modifyIORef f . handlerState

tell :: MonadHandler m => Endo [Header] -> m ()
tell hs = modify $ \g -> g { ghsHeaders = ghsHeaders g `mappend` hs }

handlerError :: MonadHandler m => HandlerContents -> m a
handlerError = liftIO . throwIO

hcError :: MonadHandler m => ErrorResponse -> m a
hcError = handlerError . HCError

getRequest :: MonadHandler m => m YesodRequest
getRequest = liftHandlerT $ HandlerT $ return . handlerRequest

runRequestBody :: MonadHandler m => m RequestBodyContents
runRequestBody = do
    HandlerData
        { handlerEnv = RunHandlerEnv {..}
        , handlerRequest = req
        } <- liftHandlerT $ HandlerT return
    let len = W.requestBodyLength $ reqWaiRequest req
        upload = rheUpload len
    x <- get
    case ghsRBC x of
        Just rbc -> return rbc
        Nothing -> do
            rr <- waiRequest
            internalState <- liftResourceT getInternalState
            rbc <- liftIO $ rbHelper upload rr internalState
            put x { ghsRBC = Just rbc }
            return rbc

rbHelper :: FileUpload -> W.Request -> InternalState -> IO RequestBodyContents
rbHelper upload req internalState =
    case upload of
        FileUploadMemory s -> rbHelper' s mkFileInfoLBS req
        FileUploadDisk s -> rbHelper' (s internalState) mkFileInfoFile req
        FileUploadSource s -> rbHelper' s mkFileInfoSource req

rbHelper' :: NWP.BackEnd x
          -> (Text -> Text -> x -> FileInfo)
          -> W.Request
          -> IO ([(Text, Text)], [(Text, FileInfo)])
rbHelper' backend mkFI req =
    (map fix1 *** mapMaybe fix2) <$> NWP.parseRequestBody backend req
  where
    fix1 = go *** go
    fix2 (x, NWP.FileInfo a' b c)
        | S.null a = Nothing
        | otherwise = Just (go x, mkFI (go a) (go b) c)
      where
        a
            | S.length a' < 2 = a'
            | S8.head a' == '"' && S8.last a' == '"' = S.tail $ S.init a'
            | S8.head a' == '\'' && S8.last a' == '\'' = S.tail $ S.init a'
            | otherwise = a'
    go = decodeUtf8With lenientDecode

askHandlerEnv :: MonadHandler m => m (RunHandlerEnv (HandlerSite m))
askHandlerEnv = liftHandlerT $ HandlerT $ return . handlerEnv

-- | Get the master site application argument.
getYesod :: MonadHandler m => m (HandlerSite m)
getYesod = rheSite <$> askHandlerEnv

-- | Get a specific component of the master site application argument.
--   Analogous to the 'gets' function for operating on 'StateT'.
getsYesod :: MonadHandler m => (HandlerSite m -> a) -> m a
getsYesod f = (f . rheSite) <$> askHandlerEnv

-- | Get the URL rendering function.
getUrlRender :: MonadHandler m => m (Route (HandlerSite m) -> Text)
getUrlRender = do
    x <- rheRender <$> askHandlerEnv
    return $ flip x []

-- | The URL rendering function with query-string parameters.
getUrlRenderParams
    :: MonadHandler m
    => m (Route (HandlerSite m) -> [(Text, Text)] -> Text)
getUrlRenderParams = rheRender <$> askHandlerEnv

-- | Get all the post parameters passed to the handler. To also get
-- the submitted files (if any), you have to use 'runRequestBody'
-- instead of this function.
--
-- @since 1.4.33
getPostParams
  :: MonadHandler m
  => m [(Text, Text)]
getPostParams = do
  reqBodyContent <- runRequestBody
  return $ fst reqBodyContent

-- | Get the route requested by the user. If this is a 404 response- where the
-- user requested an invalid route- this function will return 'Nothing'.
getCurrentRoute :: MonadHandler m => m (Maybe (Route (HandlerSite m)))
getCurrentRoute = rheRoute <$> askHandlerEnv

-- | Returns a function that runs 'HandlerT' actions inside @IO@.
--
-- Sometimes you want to run an inner 'HandlerT' action outside
-- the control flow of an HTTP request (on the outer 'HandlerT'
-- action).  For example, you may want to spawn a new thread:
--
-- @
-- getFooR :: Handler RepHtml
-- getFooR = do
--   runInnerHandler <- handlerToIO
--   liftIO $ forkIO $ runInnerHandler $ do
--     /Code here runs inside GHandler but on a new thread./
--     /This is the inner GHandler./
--     ...
--   /Code here runs inside the request's control flow./
--   /This is the outer GHandler./
--   ...
-- @
--
-- Another use case for this function is creating a stream of
-- server-sent events using 'GHandler' actions (see
-- @yesod-eventsource@).
--
-- Most of the environment from the outer 'GHandler' is preserved
-- on the inner 'GHandler', however:
--
--  * The request body is cleared (otherwise it would be very
--  difficult to prevent huge memory leaks).
--
--  * The cache is cleared (see 'CacheKey').
--
-- Changes to the response made inside the inner 'GHandler' are
-- ignored (e.g., session variables, cookies, response headers).
-- This allows the inner 'GHandler' to outlive the outer
-- 'GHandler' (e.g., on the @forkIO@ example above, a response
-- may be sent to the client without killing the new thread).
handlerToIO :: (MonadIO m1, MonadIO m2) => HandlerT site m1 (HandlerT site IO a -> m2 a)
handlerToIO =
  HandlerT $ \oldHandlerData -> do
    -- Take just the bits we need from oldHandlerData.
    let newReq = oldReq { reqWaiRequest = newWaiReq }
          where
            oldReq    = handlerRequest oldHandlerData
            oldWaiReq = reqWaiRequest oldReq
            newWaiReq = oldWaiReq { W.requestBody = return mempty
                                  , W.requestBodyLength = W.KnownLength 0
                                  }
        oldEnv = handlerEnv oldHandlerData
    newState <- liftIO $ do
      oldState <- I.readIORef (handlerState oldHandlerData)
      return $ oldState { ghsRBC = Nothing
                        , ghsIdent = 1
                        , ghsCache = mempty
                        , ghsCacheBy = mempty
                        , ghsHeaders = mempty }

    -- xx From this point onwards, no references to oldHandlerData xx
    liftIO $ evaluate (newReq `seq` oldEnv `seq` newState `seq` ())

    -- Return GHandler running function.
    return $ \(HandlerT f) ->
      liftIO $
      runResourceT $ withInternalState $ \resState -> do
        -- The state IORef needs to be created here, otherwise it
        -- will be shared by different invocations of this function.
        newStateIORef <- liftIO (I.newIORef newState)
        let newHandlerData =
              HandlerData
                { handlerRequest  = newReq
                , handlerEnv      = oldEnv
                , handlerState    = newStateIORef
                , handlerToParent = const ()
                , handlerResource = resState
                }
        liftIO (f newHandlerData)

-- | forkIO for a Handler (run an action in the background)
--
-- Uses 'handlerToIO', liftResourceT, and resourceForkIO
-- for correctness and efficiency
--
-- @since 1.2.8
forkHandler :: (SomeException -> HandlerT site IO ()) -- ^ error handler
              -> HandlerT site IO ()
              -> HandlerT site IO ()
forkHandler onErr handler = do
    yesRunner <- handlerToIO
    void $ liftResourceT $ resourceForkIO $ yesRunner $ handle onErr handler

-- | Redirect to the given route.
-- HTTP status code 303 for HTTP 1.1 clients and 302 for HTTP 1.0
-- This is the appropriate choice for a get-following-post
-- technique, which should be the usual use case.
--
-- If you want direct control of the final status code, or need a different
-- status code, please use 'redirectWith'.
redirect :: (MonadHandler m, RedirectUrl (HandlerSite m) url)
         => url -> m a
redirect url = do
    req <- waiRequest
    let status =
            if W.httpVersion req == H.http11
                then H.status303
                else H.status302
    redirectWith status url

-- | Redirect to the given URL with the specified status code.
redirectWith :: (MonadHandler m, RedirectUrl (HandlerSite m) url)
             => H.Status
             -> url
             -> m a
redirectWith status url = do
    urlText <- toTextUrl url
    handlerError $ HCRedirect status urlText

ultDestKey :: Text
ultDestKey = "_ULT"

-- | Sets the ultimate destination variable to the given route.
--
-- An ultimate destination is stored in the user session and can be loaded
-- later by 'redirectUltDest'.
setUltDest :: (MonadHandler m, RedirectUrl (HandlerSite m) url)
           => url
           -> m ()
setUltDest url = do
    urlText <- toTextUrl url
    setSession ultDestKey urlText

-- | Same as 'setUltDest', but uses the current page.
--
-- If this is a 404 handler, there is no current page, and then this call does
-- nothing.
setUltDestCurrent :: MonadHandler m => m ()
setUltDestCurrent = do
    route <- getCurrentRoute
    case route of
        Nothing -> return ()
        Just r -> do
            gets' <- reqGetParams <$> getRequest
            setUltDest (r, gets')

-- | Sets the ultimate destination to the referer request header, if present.
--
-- This function will not overwrite an existing ultdest.
setUltDestReferer :: MonadHandler m => m ()
setUltDestReferer = do
    mdest <- lookupSession ultDestKey
    maybe
        (waiRequest >>= maybe (return ()) setUltDestBS . lookup "referer" . W.requestHeaders)
        (const $ return ())
        mdest
  where
    setUltDestBS = setUltDest . T.pack . S8.unpack

-- | Redirect to the ultimate destination in the user's session. Clear the
-- value from the session.
--
-- The ultimate destination is set with 'setUltDest'.
--
-- This function uses 'redirect', and thus will perform a temporary redirect to
-- a GET request.
redirectUltDest :: (RedirectUrl (HandlerSite m) url, MonadHandler m)
                => url -- ^ default destination if nothing in session
                -> m a
redirectUltDest defaultDestination = do
    mdest <- lookupSession ultDestKey
    deleteSession ultDestKey
    maybe (redirect defaultDestination) redirect mdest

-- | Remove a previously set ultimate destination. See 'setUltDest'.
clearUltDest :: MonadHandler m => m ()
clearUltDest = deleteSession ultDestKey

msgKey :: Text
msgKey = "_MSG"

-- | Adds a status and message in the user's session.
--
-- See 'getMessages'.
--
-- @since 1.4.20
addMessage :: MonadHandler m
           => Text -- ^ status
           -> Html -- ^ message
           -> m ()
addMessage status msg = do
    val <- lookupSessionBS msgKey
    setSessionBS msgKey $ addMsg val
  where
    addMsg = maybe msg' (S.append msg' . S.cons W8._nul)
    msg' = S.append
        (encodeUtf8 status)
        (W8._nul `S.cons` L.toStrict (renderHtml msg))

-- | Adds a message in the user's session but uses RenderMessage to allow for i18n
--
-- See 'getMessages'.
--
-- @since 1.4.20
addMessageI :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
            => Text -> msg -> m ()
addMessageI status msg = do
    mr <- getMessageRender
    addMessage status $ toHtml $ mr msg

-- | Gets all messages in the user's session, and then clears the variable.
--
-- See 'addMessage'.
--
-- @since 1.4.20
getMessages :: MonadHandler m => m [(Text, Html)]
getMessages = do
    bs <- lookupSessionBS msgKey
    let ms = maybe [] enlist bs
    deleteSession msgKey
    return ms
  where
    enlist = pairup . S.split W8._nul
    pairup [] = []
    pairup [_] = []
    pairup (s:v:xs) = (decode s, preEscapedToHtml (decode v)) : pairup xs
    decode = decodeUtf8With lenientDecode

-- | Calls 'addMessage' with an empty status
setMessage :: MonadHandler m => Html -> m ()
setMessage = addMessage ""

-- | Calls 'addMessageI' with an empty status
setMessageI :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
            => msg -> m ()
setMessageI = addMessageI ""

-- | Gets just the last message in the user's session,
-- discards the rest and the status
getMessage :: MonadHandler m => m (Maybe Html)
getMessage = fmap (fmap snd . headMay) getMessages

-- | Bypass remaining handler code and output the given file.
--
-- For some backends, this is more efficient than reading in the file to
-- memory, since they can optimize file sending via a system call to sendfile.
sendFile :: MonadHandler m => ContentType -> FilePath -> m a
sendFile ct fp = handlerError $ HCSendFile ct fp Nothing

-- | Same as 'sendFile', but only sends part of a file.
sendFilePart :: MonadHandler m
             => ContentType
             -> FilePath
             -> Integer -- ^ offset
             -> Integer -- ^ count
             -> m a
sendFilePart ct fp off count = do
    fs <- liftIO $ PC.getFileStatus fp
    handlerError $ HCSendFile ct fp $ Just W.FilePart
        { W.filePartOffset = off
        , W.filePartByteCount = count
        , W.filePartFileSize = fromIntegral $ PC.fileSize fs
        }

-- | Bypass remaining handler code and output the given content with a 200
-- status code.
sendResponse :: (MonadHandler m, ToTypedContent c) => c -> m a
sendResponse = handlerError . HCContent H.status200 . toTypedContent

-- | Bypass remaining handler code and output the given content with the given
-- status code.
sendResponseStatus :: (MonadHandler m, ToTypedContent c) => H.Status -> c -> m a
sendResponseStatus s = handlerError . HCContent s . toTypedContent

-- | Bypass remaining handler code and output the given JSON with the given
-- status code.
--
-- @since 1.4.18
sendStatusJSON :: (MonadHandler m, ToJSON c) => H.Status -> c -> m a
#if MIN_VERSION_aeson(0, 11, 0)
sendStatusJSON s v = sendResponseStatus s (toEncoding v)
#else
sendStatusJSON s v = sendResponseStatus s (toJSON v)
#endif

-- | Send a 201 "Created" response with the given route as the Location
-- response header.
sendResponseCreated :: MonadHandler m => Route (HandlerSite m) -> m a
sendResponseCreated url = do
    r <- getUrlRender
    handlerError $ HCCreated $ r url

-- | Send a 'W.Response'. Please note: this function is rarely
-- necessary, and will /disregard/ any changes to response headers and session
-- that you have already specified. This function short-circuits. It should be
-- considered only for very specific needs. If you are not sure if you need it,
-- you don't.
sendWaiResponse :: MonadHandler m => W.Response -> m b
sendWaiResponse = handlerError . HCWai

-- | Switch over to handling the current request with a WAI @Application@.
--
-- @since 1.2.17
sendWaiApplication :: MonadHandler m => W.Application -> m b
sendWaiApplication = handlerError . HCWaiApp

-- | Send a raw response without conduit. This is used for cases such as
-- WebSockets. Requires WAI 3.0 or later, and a web server which supports raw
-- responses (e.g., Warp).
--
-- @since 1.2.16
sendRawResponseNoConduit
    :: (MonadHandler m, MonadBaseControl IO m)
    => (IO S8.ByteString -> (S8.ByteString -> IO ()) -> m ())
    -> m a
sendRawResponseNoConduit raw = control $ \runInIO ->
    liftIO $ throwIO $ HCWai $ flip W.responseRaw fallback
    $ \src sink -> void $ runInIO (raw src sink)
  where
    fallback = W.responseLBS H.status500 [("Content-Type", "text/plain")]
        "sendRawResponse: backend does not support raw responses"

-- | Send a raw response. This is used for cases such as WebSockets. Requires
-- WAI 2.1 or later, and a web server which supports raw responses (e.g.,
-- Warp).
--
-- @since 1.2.7
sendRawResponse :: (MonadHandler m, MonadBaseControl IO m)
                => (Source IO S8.ByteString -> Sink S8.ByteString IO () -> m ())
                -> m a
sendRawResponse raw = control $ \runInIO ->
    liftIO $ throwIO $ HCWai $ flip W.responseRaw fallback
    $ \src sink -> void $ runInIO $ raw (src' src) (CL.mapM_ sink)
  where
    fallback = W.responseLBS H.status500 [("Content-Type", "text/plain")]
        "sendRawResponse: backend does not support raw responses"
    src' src = do
        bs <- liftIO src
        unless (S.null bs) $ do
            yield bs
            src' src

-- | Send a 304 not modified response immediately. This is a short-circuiting
-- action.
--
-- @since 1.4.4
notModified :: MonadHandler m => m a
notModified = sendWaiResponse $ W.responseBuilder H.status304 [] mempty

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: MonadHandler m => m a
notFound = hcError NotFound

-- | Return a 405 method not supported page.
badMethod :: MonadHandler m => m a
badMethod = do
    w <- waiRequest
    hcError $ BadMethod $ W.requestMethod w

-- | Return a 401 status code
notAuthenticated :: MonadHandler m => m a
notAuthenticated = hcError NotAuthenticated

-- | Return a 403 permission denied page.
permissionDenied :: MonadHandler m => Text -> m a
permissionDenied = hcError . PermissionDenied

-- | Return a 403 permission denied page.
permissionDeniedI :: (RenderMessage (HandlerSite m) msg, MonadHandler m)
                  => msg
                  -> m a
permissionDeniedI msg = do
    mr <- getMessageRender
    permissionDenied $ mr msg

-- | Return a 400 invalid arguments page.
invalidArgs :: MonadHandler m => [Text] -> m a
invalidArgs = hcError . InvalidArgs

-- | Return a 400 invalid arguments page.
invalidArgsI :: (MonadHandler m, RenderMessage (HandlerSite m) msg) => [msg] -> m a
invalidArgsI msg = do
    mr <- getMessageRender
    invalidArgs $ map mr msg

------- Headers
-- | Set the cookie on the client.

setCookie :: MonadHandler m => SetCookie -> m ()
setCookie sc = do
  addHeaderInternal (DeleteCookie name path)
  addHeaderInternal (AddCookie sc)
  where name = setCookieName sc
        path = maybe "/" id (setCookiePath sc)

-- | Helper function for setCookieExpires value
getExpires :: MonadIO m
           => Int -- ^ minutes
           -> m UTCTime
getExpires m = do
    now <- liftIO getCurrentTime
    return $ fromIntegral (m * 60) `addUTCTime` now


-- | Unset the cookie on the client.
--
-- Note: although the value used for key and path is 'Text', you should only
-- use ASCII values to be HTTP compliant.
deleteCookie :: MonadHandler m
             => Text -- ^ key
             -> Text -- ^ path
             -> m ()
deleteCookie a = addHeaderInternal . DeleteCookie (encodeUtf8 a) . encodeUtf8


-- | Set the language in the user session. Will show up in 'languages' on the
-- next request.
setLanguage :: MonadHandler m => Text -> m ()
setLanguage = setSession langKey

-- | Set an arbitrary response header.
--
-- Note that, while the data type used here is 'Text', you must provide only
-- ASCII value to be HTTP compliant.
--
-- @since 1.2.0
addHeader :: MonadHandler m => Text -> Text -> m ()
addHeader a = addHeaderInternal . Header (encodeUtf8 a) . encodeUtf8

-- | Deprecated synonym for addHeader.
setHeader :: MonadHandler m => Text -> Text -> m ()
setHeader = addHeader
{-# DEPRECATED setHeader "Please use addHeader instead" #-}

-- | Replace an existing header with a new value or add a new header
-- if not present.
--
-- Note that, while the data type used here is 'Text', you must provide only
-- ASCII value to be HTTP compliant.
--
-- @since 1.4.36
replaceOrAddHeader :: MonadHandler m => Text -> Text -> m ()
replaceOrAddHeader a b =
  modify $ \g -> g {ghsHeaders = replaceHeader (ghsHeaders g)}
  where
    repHeader = Header (encodeUtf8 a) (encodeUtf8 b)

    sameHeaderName :: Header -> Header -> Bool
    sameHeaderName (Header n1 _) (Header n2 _) = T.toLower (decodeUtf8 n1) == T.toLower (decodeUtf8 n2)
    sameHeaderName _ _ = False

    replaceIndividualHeader :: [Header] -> [Header]
    replaceIndividualHeader [] = [repHeader]
    replaceIndividualHeader xs = aux xs []
      where
        aux [] acc = acc ++ [repHeader]
        aux (x:xs') acc =
          if sameHeaderName repHeader x
            then acc ++
                 [repHeader] ++
                 (filter (\header -> not (sameHeaderName header repHeader)) xs')
            else aux xs' (acc ++ [x])

    replaceHeader :: Endo [Header] -> Endo [Header]
    replaceHeader endo =
      let allHeaders :: [Header] = appEndo endo []
      in Endo (\rest -> replaceIndividualHeader allHeaders ++ rest)

-- | Set the Cache-Control header to indicate this response should be cached
-- for the given number of seconds.
cacheSeconds :: MonadHandler m => Int -> m ()
cacheSeconds i = setHeader "Cache-Control" $ T.concat
    [ "max-age="
    , T.pack $ show i
    , ", public"
    ]

-- | Set the Expires header to some date in 2037. In other words, this content
-- is never (realistically) expired.
neverExpires :: MonadHandler m => m ()
neverExpires = do
    setHeader "Expires" . rheMaxExpires =<< askHandlerEnv
    cacheSeconds oneYear
  where
    oneYear :: Int
    oneYear = 60 * 60 * 24 * 365

-- | Set an Expires header in the past, meaning this content should not be
-- cached.
alreadyExpired :: MonadHandler m => m ()
alreadyExpired = setHeader "Expires" "Thu, 01 Jan 1970 05:05:05 GMT"

-- | Set an Expires header to the given date.
expiresAt :: MonadHandler m => UTCTime -> m ()
expiresAt = setHeader "Expires" . formatRFC1123

data Etag
  = WeakEtag !S.ByteString
  -- ^ Prefixed by W/ and surrounded in quotes. Signifies that contents are
  -- semantically identical but make no guarantees about being bytewise identical.
  | StrongEtag !S.ByteString
  -- ^ Signifies that contents should be byte-for-byte identical if they match
  -- the provided ETag
  | InvalidEtag !S.ByteString
  -- ^ Anything else that ends up in a header that expects an ETag but doesn't
  -- properly follow the ETag format specified in RFC 7232, section 2.3
  deriving (Show, Eq)

-- | Check the if-none-match header and, if it matches the given value, return
-- a 304 not modified response. Otherwise, set the etag header to the given
-- value.
--
-- Note that it is the responsibility of the caller to ensure that the provided
-- value is a valid etag value, no sanity checking is performed by this
-- function.
--
-- @since 1.4.4
setEtag :: MonadHandler m => Text -> m ()
setEtag etag = do
    mmatch <- lookupHeader "if-none-match"
    let matches = maybe [] parseMatch mmatch
        baseTag = encodeUtf8 etag
        strongTag = StrongEtag baseTag
        badTag = InvalidEtag baseTag
    if any (\tag -> tag == strongTag || tag == badTag) matches
        then notModified
        else addHeader "etag" $ T.concat ["\"", etag, "\""]


-- | Parse an if-none-match field according to the spec.
parseMatch :: S.ByteString -> [Etag]
parseMatch =
    map clean . S.split W8._comma
  where
    clean = classify . fst . S.spanEnd W8.isSpace . S.dropWhile W8.isSpace

    classify bs
        | S.length bs >= 2 && S.head bs == W8._quotedbl && S.last bs == W8._quotedbl
            = StrongEtag $ S.init $ S.tail bs
        | S.length bs >= 4 &&
          S.head bs == W8._W &&
          S.index bs 1 == W8._slash &&
          S.index bs 2 == W8._quotedbl &&
          S.last bs == W8._quotedbl
            = WeakEtag $ S.init $ S.drop 3 bs
        | otherwise = InvalidEtag bs

-- | Check the if-none-match header and, if it matches the given value, return
-- a 304 not modified response. Otherwise, set the etag header to the given
-- value.
--
-- A weak etag is only expected to be semantically identical to the prior content,
-- but doesn't have to be byte-for-byte identical. Therefore it can be useful for
-- dynamically generated content that may be difficult to perform bytewise hashing
-- upon.
--
-- Note that it is the responsibility of the caller to ensure that the provided
-- value is a valid etag value, no sanity checking is performed by this
-- function.
--
-- @since 1.4.37
setWeakEtag :: MonadHandler m => Text -> m ()
setWeakEtag etag = do
    mmatch <- lookupHeader "if-none-match"
    let matches = maybe [] parseMatch mmatch
    if WeakEtag (encodeUtf8 etag) `elem` matches
        then notModified
        else addHeader "etag" $ T.concat ["W/\"", etag, "\""]

-- | Set a variable in the user's session.
--
-- The session is handled by the clientsession package: it sets an encrypted
-- and hashed cookie on the client. This ensures that all data is secure and
-- not tampered with.
setSession :: MonadHandler m
           => Text -- ^ key
           -> Text -- ^ value
           -> m ()
setSession k = setSessionBS k . encodeUtf8

-- | Same as 'setSession', but uses binary data for the value.
setSessionBS :: MonadHandler m
             => Text
             -> S.ByteString
             -> m ()
setSessionBS k = modify . modSession . Map.insert k

-- | Unsets a session variable. See 'setSession'.
deleteSession :: MonadHandler m => Text -> m ()
deleteSession = modify . modSession . Map.delete

-- | Clear all session variables.
--
-- @since: 1.0.1
clearSession :: MonadHandler m => m ()
clearSession = modify $ \x -> x { ghsSession = Map.empty }

modSession :: (SessionMap -> SessionMap) -> GHState -> GHState
modSession f x = x { ghsSession = f $ ghsSession x }

-- | Internal use only, not to be confused with 'setHeader'.
addHeaderInternal :: MonadHandler m => Header -> m ()
addHeaderInternal = tell . Endo . (:)

-- | Some value which can be turned into a URL for redirects.
class RedirectUrl master a where
    -- | Converts the value to the URL and a list of query-string parameters.
    toTextUrl :: (MonadHandler m, HandlerSite m ~ master) => a -> m Text

instance RedirectUrl master Text where
    toTextUrl = return

instance RedirectUrl master String where
    toTextUrl = toTextUrl . T.pack

instance RedirectUrl master (Route master) where
    toTextUrl url = do
        r <- getUrlRender
        return $ r url

instance (key ~ Text, val ~ Text) => RedirectUrl master (Route master, [(key, val)]) where
    toTextUrl (url, params) = do
        r <- getUrlRenderParams
        return $ r url params

instance (key ~ Text, val ~ Text) => RedirectUrl master (Route master, Map.Map key val) where
    toTextUrl (url, params) = toTextUrl (url, Map.toList params)

-- | Add a fragment identifier to a route to be used when
-- redirecting.  For example:
--
-- > redirect (NewsfeedR :#: storyId)
--
-- @since 1.2.9.
data Fragment a b = a :#: b deriving (Show, Typeable)

instance (RedirectUrl master a, PathPiece b) => RedirectUrl master (Fragment a b) where
  toTextUrl (a :#: b) = (\ua -> T.concat [ua, "#", toPathPiece b]) <$> toTextUrl a


-- | Lookup for session data.
lookupSession :: MonadHandler m => Text -> m (Maybe Text)
lookupSession = (fmap . fmap) (decodeUtf8With lenientDecode) . lookupSessionBS

-- | Lookup for session data in binary format.
lookupSessionBS :: MonadHandler m => Text -> m (Maybe S.ByteString)
lookupSessionBS n = do
    m <- fmap ghsSession get
    return $ Map.lookup n m

-- | Get all session variables.
getSession :: MonadHandler m => m SessionMap
getSession = fmap ghsSession get

-- | Get a unique identifier.
newIdent :: MonadHandler m => m Text
newIdent = do
    x <- get
    let i' = ghsIdent x + 1
    put x { ghsIdent = i' }
    return $ T.pack $ "hident" ++ show i'

-- | Redirect to a POST resource.
--
-- This is not technically a redirect; instead, it returns an HTML page with a
-- POST form, and some Javascript to automatically submit the form. This can be
-- useful when you need to post a plain link somewhere that needs to cause
-- changes on the server.
redirectToPost :: (MonadHandler m, RedirectUrl (HandlerSite m) url)
               => url
               -> m a
redirectToPost url = do
    urlText <- toTextUrl url
    req <- getRequest
    withUrlRenderer [hamlet|
$newline never
$doctype 5

<html>
    <head>
        <title>Redirecting...
    <body onload="document.getElementById('form').submit()">
        <form id="form" method="post" action=#{urlText}>
            $maybe token <- reqToken req
                <input type=hidden name=#{defaultCsrfParamName} value=#{token}>
            <noscript>
                <p>Javascript has been disabled; please click on the button below to be redirected.
            <input type="submit" value="Continue">
|] >>= sendResponse

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
hamletToRepHtml :: MonadHandler m => HtmlUrl (Route (HandlerSite m)) -> m Html
hamletToRepHtml = withUrlRenderer
{-# DEPRECATED hamletToRepHtml "Use withUrlRenderer instead" #-}

-- | Deprecated synonym for 'withUrlRenderer'.
--
-- @since 1.2.0
giveUrlRenderer :: MonadHandler m
                => ((Route (HandlerSite m) -> [(Text, Text)] -> Text) -> output)
                -> m output
giveUrlRenderer = withUrlRenderer
{-# DEPRECATED giveUrlRenderer "Use withUrlRenderer instead" #-}

-- | Provide a URL rendering function to the given function and return the
-- result. Useful for processing Shakespearean templates.
--
-- @since 1.2.20
withUrlRenderer :: MonadHandler m
                => ((Route (HandlerSite m) -> [(Text, Text)] -> Text) -> output)
                -> m output
withUrlRenderer f = do
    render <- getUrlRenderParams
    return $ f render

-- | Get the request\'s 'W.Request' value.
waiRequest :: MonadHandler m => m W.Request
waiRequest = reqWaiRequest <$> getRequest

getMessageRender :: (MonadHandler m, RenderMessage (HandlerSite m) message)
                 => m (message -> Text)
getMessageRender = do
    env <- askHandlerEnv
    l <- languages
    return $ renderMessage (rheSite env) l

-- | Use a per-request cache to avoid performing the same action multiple times.
-- Values are stored by their type, the result of typeOf from Typeable.
-- Therefore, you should use different newtype wrappers at each cache site.
--
-- For example, yesod-auth uses an un-exported newtype, CachedMaybeAuth and exports functions that utilize it such as maybeAuth.
-- This means that another module can create its own newtype wrapper to cache the same type from a different action without any cache conflicts.
--
-- See the original announcement: <http://www.yesodweb.com/blog/2013/03/yesod-1-2-cleaner-internals>
--
-- @since 1.2.0
cached :: (MonadHandler m, Typeable a)
       => m a
       -> m a
cached action = do
    cache <- ghsCache <$> get
    eres <- Cache.cached cache action
    case eres of
      Right res -> return res
      Left (newCache, res) -> do
          gs <- get
          let merged = newCache `HM.union` ghsCache gs
          put $ gs { ghsCache = merged }
          return res

-- | a per-request cache. just like 'cached'.
-- 'cached' can only cache a single value per type.
-- 'cachedBy' stores multiple values per type by usage of a ByteString key
--
-- 'cached' is ideal to cache an action that has only one value of a type, such as the session's current user
-- 'cachedBy' is required if the action has parameters and can return multiple values per type.
-- You can turn those parameters into a ByteString cache key.
-- For example, caching a lookup of a Link by a token where multiple token lookups might be performed.
--
-- @since 1.4.0
cachedBy :: (MonadHandler m, Typeable a) => S.ByteString -> m a -> m a
cachedBy k action = do
    cache <- ghsCacheBy <$> get
    eres <- Cache.cachedBy cache k action
    case eres of
      Right res -> return res
      Left (newCache, res) -> do
          gs <- get
          let merged = newCache `HM.union` ghsCacheBy gs
          put $ gs { ghsCacheBy = merged }
          return res

-- | Get the list of supported languages supplied by the user.
--
-- Languages are determined based on the following (in descending order
-- of preference):
--
-- * The _LANG user session variable.
--
-- * The _LANG get parameter.
--
-- * The _LANG cookie.
--
-- * Accept-Language HTTP header.
--
-- Yesod will seek the first language from the returned list matched with languages supporting by your application. This language will be used to render i18n templates.
-- If a matching language is not found the default language will be used.
--
-- This is handled by parseWaiRequest (not exposed).
languages :: MonadHandler m => m [Text]
languages = do
    mlang <- lookupSession langKey
    langs <- reqLangs <$> getRequest
    return $ maybe id (:) mlang langs

lookup' :: Eq a => a -> [(a, b)] -> [b]
lookup' a = map snd . filter (\x -> a == fst x)

-- | Lookup a request header.
--
-- @since 1.2.2
lookupHeader :: MonadHandler m => CI S8.ByteString -> m (Maybe S8.ByteString)
lookupHeader = fmap listToMaybe . lookupHeaders

-- | Lookup a request header.
--
-- @since 1.2.2
lookupHeaders :: MonadHandler m => CI S8.ByteString -> m [S8.ByteString]
lookupHeaders key = do
    req <- waiRequest
    return $ lookup' key $ W.requestHeaders req

-- | Lookup basic authentication data from __Authorization__ header of
-- request. Returns user name and password
--
-- @since 1.4.9
lookupBasicAuth :: (MonadHandler m) => m (Maybe (Text, Text))
lookupBasicAuth = fmap (>>= getBA) (lookupHeader "Authorization")
  where
    getBA bs = (decodeUtf8With lenientDecode *** decodeUtf8With lenientDecode)
               <$> extractBasicAuth bs

-- | Lookup bearer authentication datafrom __Authorization__ header of
-- request. Returns bearer token value
--
-- @since 1.4.9
lookupBearerAuth :: (MonadHandler m) => m (Maybe Text)
lookupBearerAuth = fmap (>>= getBR)
                   (lookupHeader "Authorization")
  where
    getBR bs = decodeUtf8With lenientDecode
               <$> extractBearerAuth bs


-- | Lookup for GET parameters.
lookupGetParams :: MonadHandler m => Text -> m [Text]
lookupGetParams pn = do
    rr <- getRequest
    return $ lookup' pn $ reqGetParams rr

-- | Lookup for GET parameters.
lookupGetParam :: MonadHandler m => Text -> m (Maybe Text)
lookupGetParam = fmap listToMaybe . lookupGetParams

-- | Lookup for POST parameters.
lookupPostParams :: (MonadResource m, MonadHandler m) => Text -> m [Text]
lookupPostParams pn = do
    (pp, _) <- runRequestBody
    return $ lookup' pn pp

lookupPostParam :: (MonadResource m, MonadHandler m)
                => Text
                -> m (Maybe Text)
lookupPostParam = fmap listToMaybe . lookupPostParams

-- | Lookup for POSTed files.
lookupFile :: MonadHandler m
           => Text
           -> m (Maybe FileInfo)
lookupFile = fmap listToMaybe . lookupFiles

-- | Lookup for POSTed files.
lookupFiles :: MonadHandler m
            => Text
            -> m [FileInfo]
lookupFiles pn = do
    (_, files) <- runRequestBody
    return $ lookup' pn files

-- | Lookup for cookie data.
lookupCookie :: MonadHandler m => Text -> m (Maybe Text)
lookupCookie = fmap listToMaybe . lookupCookies

-- | Lookup for cookie data.
lookupCookies :: MonadHandler m => Text -> m [Text]
lookupCookies pn = do
    rr <- getRequest
    return $ lookup' pn $ reqCookies rr

-- $representations
--
-- HTTP allows content negotation to determine what /representation/ of data
-- you would like to use. The most common example of this is providing both a
-- user-facing HTML page and an API facing JSON response from the same URL. The
-- means of achieving this is the Accept HTTP header, which provides a list of
-- content types the client will accept, sorted by preference.
--
-- By using 'selectRep' and 'provideRep', you can provide a number of different
-- representations, e.g.:
--
-- > selectRep $ do
-- >   provideRep produceHtmlOutput
-- >   provideRep produceJsonOutput
--
-- The first provided representation will be used if no matches are found.

-- | Select a representation to send to the client based on the representations
-- provided inside this do-block. Should be used together with 'provideRep'.
--
-- @since 1.2.0
selectRep :: MonadHandler m
          => Writer.Writer (Endo [ProvidedRep m]) ()
          -> m TypedContent
selectRep w = do
    -- the content types are already sorted by q values
    -- which have been stripped
    cts <- fmap reqAccept getRequest

    case mapMaybe tryAccept cts of
        [] ->
            case reps of
                [] -> sendResponseStatus H.status500 ("No reps provided to selectRep" :: Text)
                rep:_ ->
                  if null cts
                    then returnRep rep
                    else sendResponseStatus H.status406 explainUnaccepted
        rep:_ -> returnRep rep
  where
    explainUnaccepted :: Text
    explainUnaccepted = "no match found for accept header"

    returnRep (ProvidedRep ct mcontent) = fmap (TypedContent ct) mcontent

    reps = appEndo (Writer.execWriter w) []

    repMap = Map.unions $ map (\v@(ProvidedRep k _) -> Map.fromList
        [ (k, v)
        , (noSpace k, v)
        , (simpleContentType k, v)
        ]) reps

    -- match on the type for sub-type wildcards.
    -- If the accept is text/ * it should match a provided text/html
    mainTypeMap = Map.fromList $ reverse $ map
      (\v@(ProvidedRep ct _) -> (fst $ contentTypeTypes ct, v)) reps

    tryAccept ct =
        if subType == "*"
          then if mainType == "*"
                 then headMay reps
                 else Map.lookup mainType mainTypeMap
          else lookupAccept ct
        where
          (mainType, subType) = contentTypeTypes ct

    lookupAccept ct = Map.lookup ct repMap <|>
                      Map.lookup (noSpace ct) repMap <|>
                      Map.lookup (simpleContentType ct) repMap

    -- Mime types such as "text/html; charset=foo" get converted to
    -- "text/html;charset=foo"
    noSpace = S8.filter (/= ' ')

-- | Internal representation of a single provided representation.
--
-- @since 1.2.0
data ProvidedRep m = ProvidedRep !ContentType !(m Content)

-- | Provide a single representation to be used, based on the request of the
-- client. Should be used together with 'selectRep'.
--
-- @since 1.2.0
provideRep :: (Monad m, HasContentType a)
           => m a
           -> Writer.Writer (Endo [ProvidedRep m]) ()
provideRep handler = provideRepType (getContentType handler) handler

-- | Same as 'provideRep', but instead of determining the content type from the
-- type of the value itself, you provide the content type separately. This can
-- be a convenience instead of creating newtype wrappers for uncommonly used
-- content types.
--
-- > provideRepType "application/x-special-format" "This is the content"
--
-- @since 1.2.0
provideRepType :: (Monad m, ToContent a)
               => ContentType
               -> m a
               -> Writer.Writer (Endo [ProvidedRep m]) ()
provideRepType ct handler =
    Writer.tell $ Endo (ProvidedRep ct (liftM toContent handler):)

-- | Stream in the raw request body without any parsing.
--
-- @since 1.2.0
rawRequestBody :: MonadHandler m => Source m S.ByteString
rawRequestBody = do
    req <- lift waiRequest
    let loop = do
            bs <- liftIO $ W.requestBody req
            unless (S.null bs) $ do
                yield bs
                loop
    loop

-- | Stream the data from the file. Since Yesod 1.2, this has been generalized
-- to work in any @MonadResource@.
fileSource :: MonadResource m => FileInfo -> Source m S.ByteString
fileSource = transPipe liftResourceT . fileSourceRaw

-- | Provide a pure value for the response body.
--
-- > respond ct = return . TypedContent ct . toContent
--
-- @since 1.2.0
respond :: (Monad m, ToContent a) => ContentType -> a -> m TypedContent
respond ct = return . TypedContent ct . toContent

-- | Use a @Source@ for the response body.
--
-- Note that, for ease of use, the underlying monad is a @HandlerT@. This
-- implies that you can run any @HandlerT@ action. However, since a streaming
-- response occurs after the response headers have already been sent, some
-- actions make no sense here. For example: short-circuit responses, setting
-- headers, changing status codes, etc.
--
-- @since 1.2.0
respondSource :: ContentType
              -> Source (HandlerT site IO) (Flush Builder)
              -> HandlerT site IO TypedContent
respondSource ctype src = HandlerT $ \hd ->
    -- Note that this implementation relies on the fact that the ResourceT
    -- environment provided by the server is the same one used in HandlerT.
    -- This is a safe assumption assuming the HandlerT is run correctly.
    return $ TypedContent ctype $ ContentSource
           $ transPipe (lift . flip unHandlerT hd) src

-- | In a streaming response, send a single chunk of data. This function works
-- on most datatypes, such as @ByteString@ and @Html@.
--
-- @since 1.2.0
sendChunk :: Monad m => ToFlushBuilder a => a -> Producer m (Flush Builder)
sendChunk = yield . toFlushBuilder

-- | In a streaming response, send a flush command, causing all buffered data
-- to be immediately sent to the client.
--
-- @since 1.2.0
sendFlush :: Monad m => Producer m (Flush Builder)
sendFlush = yield Flush

-- | Type-specialized version of 'sendChunk' for strict @ByteString@s.
--
-- @since 1.2.0
sendChunkBS :: Monad m => S.ByteString -> Producer m (Flush Builder)
sendChunkBS = sendChunk

-- | Type-specialized version of 'sendChunk' for lazy @ByteString@s.
--
-- @since 1.2.0
sendChunkLBS :: Monad m => L.ByteString -> Producer m (Flush Builder)
sendChunkLBS = sendChunk

-- | Type-specialized version of 'sendChunk' for strict @Text@s.
--
-- @since 1.2.0
sendChunkText :: Monad m => T.Text -> Producer m (Flush Builder)
sendChunkText = sendChunk

-- | Type-specialized version of 'sendChunk' for lazy @Text@s.
--
-- @since 1.2.0
sendChunkLazyText :: Monad m => TL.Text -> Producer m (Flush Builder)
sendChunkLazyText = sendChunk

-- | Type-specialized version of 'sendChunk' for @Html@s.
--
-- @since 1.2.0
sendChunkHtml :: Monad m => Html -> Producer m (Flush Builder)
sendChunkHtml = sendChunk

-- | Converts a child handler to a parent handler
--
-- Exported since 1.4.11
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

-- $ajaxCSRFOverview
-- When a user has authenticated with your site, all requests made from the browser to your server will include the session information that you use to verify that the user is logged in.
-- Unfortunately, this allows attackers to make unwanted requests on behalf of the user by e.g. submitting an HTTP request to your site when the user visits theirs.
-- This is known as a <https://en.wikipedia.org/wiki/Cross-site_request_forgery Cross Site Request Forgery> (CSRF) attack.
--
-- To combat this attack, you need a way to verify that the request is valid.
-- This is achieved by generating a random string ("token"), storing it in your encrypted session so that the server can look it up (see 'reqToken'), and adding the token to HTTP requests made to your server.
-- When a request comes in, the token in the request is compared to the one from the encrypted session. If they match, you can be sure the request is valid.
--
-- Yesod implements this behavior in two ways:
--
-- (1) The yesod-form package <http://www.yesodweb.com/book/forms#forms_running_forms stores the CSRF token in a hidden field> in the form, then validates it with functions like 'Yesod.Form.Functions.runFormPost'.
--
-- (2) Yesod can store the CSRF token in a cookie which is accessible by Javascript. Requests made by Javascript can lookup this cookie and add it as a header to requests. The server then checks the token in the header against the one in the encrypted session.
--
-- The form-based approach has the advantage of working for users with Javascript disabled, while adding the token to the headers with Javascript allows things like submitting JSON or binary data in AJAX requests. Yesod supports checking for a CSRF token in either the POST parameters of the form ('checkCsrfParamNamed'), the headers ('checkCsrfHeaderNamed'), or both options ('checkCsrfHeaderOrParam').
--
-- The easiest way to check both sources is to add the 'Yesod.Core.defaultCsrfMiddleware' to your Yesod Middleware.
--
-- === Opting-out of CSRF checking for specific routes
--
-- (Note: this code is generic to opting out of any Yesod middleware)
--
-- @
-- 'yesodMiddleware' app = do
--   maybeRoute <- 'getCurrentRoute'
--   let dontCheckCsrf = case maybeRoute of
--                         Just HomeR                     -> True  -- Don't check HomeR
--                         Nothing                        -> True  -- Don't check for 404s
--                         _                              -> False -- Check other routes
--
--   'defaultYesodMiddleware' $ 'defaultCsrfSetCookieMiddleware' $ (if dontCheckCsrf then 'id' else 'defaultCsrfCheckMiddleware') $ app
-- @
--
-- This can also be implemented using the 'csrfCheckMiddleware' function.

-- | The default cookie name for the CSRF token ("XSRF-TOKEN").
--
-- @since 1.4.14
defaultCsrfCookieName :: S8.ByteString
defaultCsrfCookieName = "XSRF-TOKEN"

-- | Sets a cookie with a CSRF token, using 'defaultCsrfCookieName' for the cookie name.
--
-- The cookie's path is set to @/@, making it valid for your whole website.
--
-- @since 1.4.14
setCsrfCookie :: MonadHandler m => m ()
setCsrfCookie = setCsrfCookieWithCookie def { setCookieName = defaultCsrfCookieName, setCookiePath = Just "/" }

-- | Takes a 'SetCookie' and overrides its value with a CSRF token, then sets the cookie.
--
-- Make sure to set the 'setCookiePath' to the root path of your application, otherwise you'll generate a new CSRF token for every path of your app. If your app is run from from e.g. www.example.com\/app1, use @app1@. The vast majority of sites will just use @/@.
--
-- @since 1.4.14
setCsrfCookieWithCookie :: MonadHandler m => SetCookie -> m ()
setCsrfCookieWithCookie cookie  = do
    mCsrfToken <- reqToken <$> getRequest
    Fold.forM_ mCsrfToken (\token -> setCookie $ cookie { setCookieValue = encodeUtf8 token })

-- | The default header name for the CSRF token ("X-XSRF-TOKEN").
--
-- @since 1.4.14
defaultCsrfHeaderName :: CI S8.ByteString
defaultCsrfHeaderName = "X-XSRF-TOKEN"

-- | Takes a header name to lookup a CSRF token. If the value doesn't match the token stored in the session,
-- this function throws a 'PermissionDenied' error.
--
-- @since 1.4.14
checkCsrfHeaderNamed :: MonadHandler m => CI S8.ByteString -> m ()
checkCsrfHeaderNamed headerName = do
  (valid, mHeader) <- hasValidCsrfHeaderNamed' headerName
  unless valid (permissionDenied $ csrfErrorMessage [CSRFHeader (decodeUtf8 $ original headerName) mHeader])

-- | Takes a header name to lookup a CSRF token, and returns whether the value matches the token stored in the session.
--
-- @since 1.4.14
hasValidCsrfHeaderNamed :: MonadHandler m => CI S8.ByteString -> m Bool
hasValidCsrfHeaderNamed headerName = fst <$> hasValidCsrfHeaderNamed' headerName

-- | Like 'hasValidCsrfHeaderNamed', but also returns the header value to be used in error messages.
hasValidCsrfHeaderNamed' :: MonadHandler m => CI S8.ByteString -> m (Bool, Maybe Text)
hasValidCsrfHeaderNamed' headerName = do
  mCsrfToken  <- reqToken <$> getRequest
  mXsrfHeader <- lookupHeader headerName

  return $ (validCsrf mCsrfToken mXsrfHeader, decodeUtf8 <$> mXsrfHeader)

-- CSRF Parameter checking

-- | The default parameter name for the CSRF token ("_token")
--
-- @since 1.4.14
defaultCsrfParamName :: Text
defaultCsrfParamName = "_token"

-- | Takes a POST parameter name to lookup a CSRF token. If the value doesn't match the token stored in the session,
-- this function throws a 'PermissionDenied' error.
--
-- @since 1.4.14
checkCsrfParamNamed :: MonadHandler m => Text -> m ()
checkCsrfParamNamed paramName = do
  (valid, mParam) <- hasValidCsrfParamNamed' paramName
  unless valid (permissionDenied $ csrfErrorMessage [CSRFParam paramName mParam])

-- | Takes a POST parameter name to lookup a CSRF token, and returns whether the value matches the token stored in the session.
--
-- @since 1.4.14
hasValidCsrfParamNamed :: MonadHandler m => Text -> m Bool
hasValidCsrfParamNamed paramName = fst <$> hasValidCsrfParamNamed' paramName

-- | Like 'hasValidCsrfParamNamed', but also returns the param value to be used in error messages.
hasValidCsrfParamNamed' :: MonadHandler m => Text -> m (Bool, Maybe Text)
hasValidCsrfParamNamed' paramName = do
  mCsrfToken  <- reqToken <$> getRequest
  mCsrfParam <- lookupPostParam paramName

  return $ (validCsrf mCsrfToken (encodeUtf8 <$> mCsrfParam), mCsrfParam)

-- | Checks that a valid CSRF token is present in either the request headers or POST parameters.
-- If the value doesn't match the token stored in the session, this function throws a 'PermissionDenied' error.
--
-- @since 1.4.14
checkCsrfHeaderOrParam :: (MonadHandler m, MonadLogger m)
                       => CI S8.ByteString -- ^ The header name to lookup the CSRF token
                       -> Text -- ^ The POST parameter name to lookup the CSRF token
                       -> m ()
checkCsrfHeaderOrParam headerName paramName = do
  (validHeader, mHeader) <- hasValidCsrfHeaderNamed' headerName
  (validParam, mParam) <- hasValidCsrfParamNamed' paramName
  unless (validHeader || validParam) $ do
    let errorMessage = csrfErrorMessage $ [CSRFHeader (decodeUtf8 $ original headerName) mHeader, CSRFParam paramName mParam]
    $logWarnS "yesod-core" errorMessage
    permissionDenied errorMessage

validCsrf :: Maybe Text -> Maybe S.ByteString -> Bool
-- It's important to use constant-time comparison (constEqBytes) in order to avoid timing attacks.
validCsrf (Just token) (Just param) = encodeUtf8 token `constEqBytes` param
validCsrf Nothing            _param = True
validCsrf (Just _token)     Nothing = False

data CSRFExpectation = CSRFHeader Text (Maybe Text) -- Key/Value
                     | CSRFParam Text (Maybe Text) -- Key/Value

csrfErrorMessage :: [CSRFExpectation]
                  -> Text -- ^ Error message
csrfErrorMessage expectedLocations = T.intercalate "\n"
  [ "A valid CSRF token wasn't present. Because the request could have been forged, it's been rejected altogether."
  , "If you're a developer of this site, these tips will help you debug the issue:"
  , "- Read the Yesod.Core.Handler docs of the yesod-core package for details on CSRF protection."
  , "- Check that your HTTP client is persisting cookies between requests, like a browser does."
  , "- By default, the CSRF token is sent to the client in a cookie named " `mappend` (decodeUtf8 defaultCsrfCookieName) `mappend` "."
  , "- The server is looking for the token in the following locations:\n" `mappend` T.intercalate "\n" (map csrfLocation expectedLocations)
  ]

  where csrfLocation expected = case expected of
          CSRFHeader k v -> T.intercalate " " ["  - An HTTP header named", k, (formatValue v)]
          CSRFParam k v -> T.intercalate " " ["  - A POST parameter named", k, (formatValue v)]

        formatValue :: Maybe Text -> Text
        formatValue maybeText = case maybeText of
          Nothing -> "(which is not currently set)"
          Just t -> T.concat ["(which has the current, incorrect value: '", t, "')"]
