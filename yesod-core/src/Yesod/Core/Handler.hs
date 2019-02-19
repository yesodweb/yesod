{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE QuantifiedConstraints      #-}
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
    , HandlerFor
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
    , fileSourceByteString
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
    , sendResponseNoContent
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
    , addContentDispositionFileName
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
      -- * Subsites
    , SubHandlerFor
    , getSubYesod
    , getRouteToParent
    , getSubCurrentRoute
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
    , cacheGet
    , cacheSet
    , cachedBy
    , cacheByGet
    , cacheBySet
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


import           Control.Applicative           ((<|>))
import qualified Data.CaseInsensitive          as CI

import           Control.Monad                 (void, liftM, unless)
import qualified Control.Monad.Trans.Writer    as Writer

import           RIO
import           RIO.Orphans

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

import           Data.ByteArray                (constEq)

import           Control.Arrow                 ((***))
import qualified Data.ByteString.Char8         as S8
import           Data.Monoid                   (Endo (..))
import           Data.Text                     (Text)
import qualified Network.Wai.Parse             as NWP
import           Text.Shakespeare.I18N         (RenderMessage (..))
import           Web.Cookie                    (SetCookie (..), defaultSetCookie)
import           Yesod.Core.Content            (ToTypedContent (..), simpleContentType, contentTypeTypes, HasContentType (..), ToContent (..), ToFlushBuilder (..))
import           Yesod.Core.Internal.Util      (formatRFC1123)
import           Text.Blaze.Html               (preEscapedToHtml, toHtml)

import           Data.Maybe                    (listToMaybe, mapMaybe)
import           Data.Typeable                 (Typeable)
import           Web.PathPieces                (PathPiece(..))
import           Yesod.Core.Types
import           Yesod.Routes.Class            (Route)
import           Data.ByteString.Builder (Builder)
import           Data.CaseInsensitive (CI, original)
import qualified Data.Conduit.List as CL
import           Control.Monad.Trans.Resource  (MonadResource, InternalState, getInternalState, liftResourceT, resourceForkIO)
import qualified System.PosixCompat.Files as PC
import           Conduit ((.|), runConduit, sinkLazy)
import           Data.Conduit (ConduitT, transPipe, Flush (Flush), yield, Void)
import qualified Yesod.Core.TypeCache as Cache
import qualified Data.Word8 as W8
import qualified Data.Foldable as Fold

type HandlerT site (m :: * -> *) = HandlerFor site
{-# DEPRECATED HandlerT "Use HandlerFor directly" #-}

get :: HasHandlerData env => RIO env GHState
get = view (subHandlerDataL.to handlerState) >>= readIORef

put :: HasHandlerData env => GHState -> RIO env ()
put x = view (subHandlerDataL.to handlerState) >>= flip writeIORef x

modify :: HasHandlerData env => (GHState -> GHState) -> RIO env ()
modify f = view (subHandlerDataL.to handlerState) >>= flip modifyIORef f

tell :: HasHandlerData env => Endo [Header] -> RIO env ()
tell hs = modify $ \g -> g { ghsHeaders = ghsHeaders g `mappend` hs }

handlerError :: HasHandlerData env => HandlerContents -> RIO env a
handlerError = liftIO . throwIO

hcError :: HasHandlerData env => ErrorResponse -> RIO env a
hcError = handlerError . HCError

getRequest :: HasHandlerData env => RIO env YesodRequest
getRequest = view $ subHandlerDataL.to handlerRequest

runRequestBody :: HasHandlerData env => RIO env RequestBodyContents
runRequestBody = do
    SubHandlerData
        { handlerEnv = RunHandlerEnv {..}
        , handlerRequest = req
        } <- view subHandlerDataL
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

askHandlerEnv :: HasHandlerData env => RIO env (RunHandlerEnv (SubHandlerSite env) (HandlerSite env))
askHandlerEnv = view $ subHandlerDataL.to handlerEnv

-- | Get the master site application argument.
getYesod :: HasHandlerData env => RIO env (HandlerSite env)
getYesod = rheSite <$> askHandlerEnv

-- | Get a specific component of the master site application argument.
--   Analogous to the 'gets' function for operating on 'StateT'.
getsYesod :: HasHandlerData env => (HandlerSite env -> a) -> RIO env a
getsYesod f = (f . rheSite) <$> askHandlerEnv

-- | Get the URL rendering function.
getUrlRender :: HasHandlerData env => RIO env (Route (HandlerSite env) -> Text)
getUrlRender = do
    x <- rheRender <$> askHandlerEnv
    return $ flip x []

-- | The URL rendering function with query-string parameters.
getUrlRenderParams
    :: HasHandlerData env
    => RIO env (Route (HandlerSite env) -> [(Text, Text)] -> Text)
getUrlRenderParams = rheRender <$> askHandlerEnv

-- | Get all the post parameters passed to the handler. To also get
-- the submitted files (if any), you have to use 'runRequestBody'
-- instead of this function.
--
-- @since 1.4.33
getPostParams
  :: HasHandlerData env
  => RIO env [(Text, Text)]
getPostParams = do
  reqBodyContent <- runRequestBody
  return $ fst reqBodyContent

-- | Get the route requested by the user. If this is a 404 response- where the
-- user requested an invalid route- this function will return 'Nothing'.
getCurrentRoute :: HasHandlerData env => RIO env (Maybe (Route (HandlerSite env)))
getCurrentRoute = do
  rhe <- askHandlerEnv
  pure $ rheRouteToMaster rhe <$> rheRoute rhe

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
handlerToIO :: MonadIO m => HandlerFor site (HandlerFor site a -> m a)
handlerToIO = do
    oldHandlerData <- view subHandlerDataL
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
      oldState <- readIORef (handlerState oldHandlerData)
      return $ oldState { ghsRBC = Nothing
                        , ghsIdent = 1
                        , ghsCache = mempty
                        , ghsCacheBy = mempty
                        , ghsHeaders = mempty }

    -- xx From this point onwards, no references to oldHandlerData xx
    liftIO $ evaluate (newReq `seq` oldEnv `seq` newState `seq` ())

    -- Return GHandler running function.
    return $ \action ->
      liftIO $
      withResourceMap $ \resourceMap -> do
        -- The state IORef needs to be created here, otherwise it
        -- will be shared by different invocations of this function.
        newStateIORef <- liftIO (newIORef newState)
        let newHandlerData =
              HandlerData $ SubHandlerData
                { handlerRequest  = newReq
                , handlerEnv      = oldEnv
                , handlerState    = newStateIORef
                , handlerResource = resourceMap
                }
        runRIO newHandlerData action

-- | forkIO for a Handler (run an action in the background)
--
-- Uses 'handlerToIO', liftResourceT, and resourceForkIO
-- for correctness and efficiency
--
-- @since 1.2.8
forkHandler :: (SomeException -> HandlerFor site ()) -- ^ error handler
              -> HandlerFor site ()
              -> HandlerFor site ()
forkHandler onErr handler = do
    yesRunner <- handlerToIO
    void $ liftResourceT $ resourceForkIO $
      liftIO $ handle (yesRunner . onErr) (yesRunner handler)

-- | Redirect to the given route.
-- HTTP status code 303 for HTTP 1.1 clients and 302 for HTTP 1.0
-- This is the appropriate choice for a get-following-post
-- technique, which should be the usual use case.
--
-- If you want direct control of the final status code, or need a different
-- status code, please use 'redirectWith'.
redirect :: (HasHandlerData env, RedirectUrl (HandlerSite env) url)
         => url -> RIO env a
redirect url = do
    req <- waiRequest
    let status =
            if W.httpVersion req == H.http11
                then H.status303
                else H.status302
    redirectWith status url

-- | Redirect to the given URL with the specified status code.
redirectWith :: (HasHandlerData env, RedirectUrl (HandlerSite env) url)
             => H.Status
             -> url
             -> RIO env a
redirectWith status url = do
    urlText <- toTextUrl url
    handlerError $ HCRedirect status urlText

ultDestKey :: Text
ultDestKey = "_ULT"

-- | Sets the ultimate destination variable to the given route.
--
-- An ultimate destination is stored in the user session and can be loaded
-- later by 'redirectUltDest'.
setUltDest :: (HasHandlerData env, RedirectUrl (HandlerSite env) url)
           => url
           -> RIO env ()
setUltDest url = do
    urlText <- toTextUrl url
    setSession ultDestKey urlText

-- | Same as 'setUltDest', but uses the current page.
--
-- If this is a 404 handler, there is no current page, and then this call does
-- nothing.
setUltDestCurrent :: HasHandlerData env => RIO env ()
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
setUltDestReferer :: HasHandlerData env => RIO env ()
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
redirectUltDest :: (RedirectUrl (HandlerSite env) url, HasHandlerData env)
                => url -- ^ default destination if nothing in session
                -> RIO env a
redirectUltDest defaultDestination = do
    mdest <- lookupSession ultDestKey
    deleteSession ultDestKey
    maybe (redirect defaultDestination) redirect mdest

-- | Remove a previously set ultimate destination. See 'setUltDest'.
clearUltDest :: HasHandlerData env => RIO env ()
clearUltDest = deleteSession ultDestKey

msgKey :: Text
msgKey = "_MSG"

-- | Adds a status and message in the user's session.
--
-- See 'getMessages'.
--
-- @since 1.4.20
addMessage :: HasHandlerData env
           => Text -- ^ status
           -> Html -- ^ message
           -> RIO env ()
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
addMessageI :: (HasHandlerData env, RenderMessage (HandlerSite env) msg)
            => Text -> msg -> RIO env ()
addMessageI status msg = do
    mr <- getMessageRender
    addMessage status $ toHtml $ mr msg

-- | Gets all messages in the user's session, and then clears the variable.
--
-- See 'addMessage'.
--
-- @since 1.4.20
getMessages :: HasHandlerData env => RIO env [(Text, Html)]
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
setMessage :: HasHandlerData env => Html -> RIO env ()
setMessage = addMessage ""

-- | Calls 'addMessageI' with an empty status
setMessageI :: (HasHandlerData env, RenderMessage (HandlerSite env) msg)
            => msg
            -> RIO env ()
setMessageI = addMessageI ""

-- | Gets just the last message in the user's session,
-- discards the rest and the status
getMessage :: HasHandlerData env => RIO env (Maybe Html)
getMessage = fmap (fmap snd . listToMaybe) getMessages

-- | Bypass remaining handler code and output the given file.
--
-- For some backends, this is more efficient than reading in the file to
-- memory, since they can optimize file sending via a system call to sendfile.
sendFile :: HasHandlerData env => ContentType -> FilePath -> RIO env a
sendFile ct fp = handlerError $ HCSendFile ct fp Nothing

-- | Same as 'sendFile', but only sends part of a file.
sendFilePart :: HasHandlerData env
             => ContentType
             -> FilePath
             -> Integer -- ^ offset
             -> Integer -- ^ count
             -> RIO env a
sendFilePart ct fp off count = do
    fs <- liftIO $ PC.getFileStatus fp
    handlerError $ HCSendFile ct fp $ Just W.FilePart
        { W.filePartOffset = off
        , W.filePartByteCount = count
        , W.filePartFileSize = fromIntegral $ PC.fileSize fs
        }

-- | Bypass remaining handler code and output the given content with a 200
-- status code.
sendResponse :: (HasHandlerData env, ToTypedContent c) => c -> RIO env a
sendResponse = handlerError . HCContent H.status200 . toTypedContent

-- | Bypass remaining handler code and output the given content with the given
-- status code.
sendResponseStatus :: (HasHandlerData env, ToTypedContent c) => H.Status -> c -> RIO env a
sendResponseStatus s = handlerError . HCContent s . toTypedContent

-- | Bypass remaining handler code and output the given JSON with the given
-- status code.
--
-- @since 1.4.18
sendStatusJSON :: (HasHandlerData env, ToJSON c) => H.Status -> c -> RIO env a
sendStatusJSON s v = sendResponseStatus s (toEncoding v)

-- | Send a 201 "Created" response with the given route as the Location
-- response header.
sendResponseCreated :: HasHandlerData env => Route (HandlerSite env) -> RIO env a
sendResponseCreated url = do
    r <- getUrlRender
    handlerError $ HCCreated $ r url

-- | Bypass remaining handler code and output no content with a 204 status code.
--
-- @since 1.6.9
sendResponseNoContent :: HasHandlerData env => RIO env a
sendResponseNoContent = sendWaiResponse $ W.responseBuilder H.status204 [] mempty

-- | Send a 'W.Response'. Please note: this function is rarely
-- necessary, and will /disregard/ any changes to response headers and session
-- that you have already specified. This function short-circuits. It should be
-- considered only for very specific needs. If you are not sure if you need it,
-- you don't.
sendWaiResponse :: HasHandlerData env => W.Response -> RIO env b
sendWaiResponse = handlerError . HCWai

-- | Switch over to handling the current request with a WAI @Application@.
--
-- @since 1.2.17
sendWaiApplication :: HasHandlerData env => W.Application -> RIO env b
sendWaiApplication = handlerError . HCWaiApp

-- | Send a raw response without conduit. This is used for cases such as
-- WebSockets. Requires WAI 3.0 or later, and a web server which supports raw
-- responses (e.g., Warp).
--
-- @since 1.2.16
sendRawResponseNoConduit
    :: HasHandlerData env
    => (IO S8.ByteString -> (S8.ByteString -> IO ()) -> RIO env ())
    -> RIO env a
sendRawResponseNoConduit raw = withRunInIO $ \runInIO ->
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
sendRawResponse
  :: HasHandlerData env
  => (ConduitT () S8.ByteString IO () -> ConduitT S8.ByteString Void IO () -> RIO env ())
  -> RIO env a
sendRawResponse raw = withRunInIO $ \runInIO ->
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
notModified :: HasHandlerData env => RIO env a
notModified = sendWaiResponse $ W.responseBuilder H.status304 [] mempty

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: HasHandlerData env => RIO env a
notFound = hcError NotFound

-- | Return a 405 method not supported page.
badMethod :: HasHandlerData env => RIO env a
badMethod = do
    w <- waiRequest
    hcError $ BadMethod $ W.requestMethod w

-- | Return a 401 status code
notAuthenticated :: HasHandlerData env => RIO env a
notAuthenticated = hcError NotAuthenticated

-- | Return a 403 permission denied page.
permissionDenied :: HasHandlerData env => Utf8Builder -> RIO env a
permissionDenied = hcError . PermissionDenied . utf8BuilderToText -- FIXME inefficient

-- | Return a 403 permission denied page.
permissionDeniedI :: (RenderMessage (HandlerSite env) msg, HasHandlerData env)
                  => msg
                  -> RIO env a
permissionDeniedI msg = do
    mr <- getMessageRender
    permissionDenied $ display $ mr msg

-- | Return a 400 invalid arguments page.
invalidArgs :: HasHandlerData env => [Text] -> RIO env a
invalidArgs = hcError . InvalidArgs

-- | Return a 400 invalid arguments page.
invalidArgsI :: (HasHandlerData env, RenderMessage (HandlerSite env) msg) => [msg] -> RIO env a
invalidArgsI msg = do
    mr <- getMessageRender
    invalidArgs $ map mr msg

------- Headers
-- | Set the cookie on the client.

setCookie :: HasHandlerData env => SetCookie -> RIO env ()
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
deleteCookie :: HasHandlerData env
             => Text -- ^ key
             -> Text -- ^ path
             -> RIO env ()
deleteCookie a = addHeaderInternal . DeleteCookie (encodeUtf8 a) . encodeUtf8


-- | Set the language in the user session. Will show up in 'languages' on the
-- next request.
setLanguage :: HasHandlerData env => Text -> RIO env ()
setLanguage = setSession langKey

-- | Set attachment file name.
--
-- Allows Unicode characters by encoding to UTF-8.
-- Some modurn browser parse UTF-8 characters with out encoding setting.
-- But, for example IE9 can't parse UTF-8 characters.
-- This function use
-- <https://tools.ietf.org/html/rfc6266 RFC 6266>(<https://tools.ietf.org/html/rfc5987 RFC 5987>)
--
-- @since 1.6.4
addContentDispositionFileName :: HasHandlerData env => T.Text -> RIO env ()
addContentDispositionFileName fileName
    = addHeader "Content-Disposition" $ rfc6266Utf8FileName fileName

-- | <https://tools.ietf.org/html/rfc6266 RFC 6266> Unicode attachment filename.
--
-- > rfc6266Utf8FileName (Data.Text.pack "€")
-- "attachment; filename*=UTF-8''%E2%82%AC"
rfc6266Utf8FileName :: T.Text -> T.Text
rfc6266Utf8FileName fileName = "attachment; filename*=UTF-8''" `mappend` decodeUtf8 (H.urlEncode True (encodeUtf8 fileName))

-- | Set an arbitrary response header.
--
-- Note that, while the data type used here is 'Text', you must provide only
-- ASCII value to be HTTP compliant.
--
-- @since 1.2.0
addHeader :: HasHandlerData env => Text -> Text -> RIO env ()
addHeader a = addHeaderInternal . Header (CI.mk $ encodeUtf8 a) . encodeUtf8

-- | Deprecated synonym for addHeader.
setHeader :: HasHandlerData env => Text -> Text -> RIO env ()
setHeader = addHeader
{-# DEPRECATED setHeader "Please use addHeader instead" #-}

-- | Replace an existing header with a new value or add a new header
-- if not present.
--
-- Note that, while the data type used here is 'Text', you must provide only
-- ASCII value to be HTTP compliant.
--
-- @since 1.4.36
replaceOrAddHeader :: HasHandlerData env => Text -> Text -> RIO env ()
replaceOrAddHeader a b =
  modify $ \g -> g {ghsHeaders = replaceHeader (ghsHeaders g)}
  where
    repHeader = Header (CI.mk $ encodeUtf8 a) (encodeUtf8 b)

    sameHeaderName :: Header -> Header -> Bool
    sameHeaderName (Header n1 _) (Header n2 _) = n1 == n2
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
cacheSeconds :: HasHandlerData env => Int -> RIO env ()
cacheSeconds i = setHeader "Cache-Control" $ T.concat
    [ "max-age="
    , T.pack $ show i
    , ", public"
    ]

-- | Set the Expires header to some date in 2037. In other words, this content
-- is never (realistically) expired.
neverExpires :: HasHandlerData env => RIO env ()
neverExpires = do
    setHeader "Expires" . rheMaxExpires =<< askHandlerEnv
    cacheSeconds oneYear
  where
    oneYear :: Int
    oneYear = 60 * 60 * 24 * 365

-- | Set an Expires header in the past, meaning this content should not be
-- cached.
alreadyExpired :: HasHandlerData env => RIO env ()
alreadyExpired = setHeader "Expires" "Thu, 01 Jan 1970 05:05:05 GMT"

-- | Set an Expires header to the given date.
expiresAt :: HasHandlerData env => UTCTime -> RIO env ()
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
setEtag :: HasHandlerData env => Text -> RIO env ()
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
setWeakEtag :: HasHandlerData env => Text -> RIO env ()
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
setSession :: HasHandlerData env
           => Text -- ^ key
           -> Text -- ^ value
           -> RIO env ()
setSession k = setSessionBS k . encodeUtf8

-- | Same as 'setSession', but uses binary data for the value.
setSessionBS :: HasHandlerData env
             => Text
             -> S.ByteString
             -> RIO env ()
setSessionBS k = modify . modSession . Map.insert k

-- | Unsets a session variable. See 'setSession'.
deleteSession :: HasHandlerData env => Text -> RIO env ()
deleteSession = modify . modSession . Map.delete

-- | Clear all session variables.
--
-- @since: 1.0.1
clearSession :: HasHandlerData env => RIO env ()
clearSession = modify $ \x -> x { ghsSession = Map.empty }

modSession :: (SessionMap -> SessionMap) -> GHState -> GHState
modSession f x = x { ghsSession = f $ ghsSession x }

-- | Internal use only, not to be confused with 'setHeader'.
addHeaderInternal :: HasHandlerData env => Header -> RIO env ()
addHeaderInternal = tell . Endo . (:)

-- | Some value which can be turned into a URL for redirects.
class RedirectUrl master a where
    -- | Converts the value to the URL and a list of query-string parameters.
    toTextUrl :: (HandlerSite env ~ master, HasHandlerData env) => a -> RIO env Text

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
lookupSession :: HasHandlerData env => Text -> RIO env (Maybe Text)
lookupSession = (fmap . fmap) (decodeUtf8With lenientDecode) . lookupSessionBS

-- | Lookup for session data in binary format.
lookupSessionBS :: HasHandlerData env => Text -> RIO env (Maybe S.ByteString)
lookupSessionBS n = do
    m <- fmap ghsSession get
    return $ Map.lookup n m

-- | Get all session variables.
getSession :: HasHandlerData env => RIO env SessionMap
getSession = fmap ghsSession get

-- | Get a unique identifier.
newIdent :: HasHandlerData env => RIO env Text
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
redirectToPost :: (HasHandlerData env, RedirectUrl (HandlerSite env) url)
               => url
               -> RIO env a
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
hamletToRepHtml :: HasHandlerData env => HtmlUrl (Route (HandlerSite env)) -> RIO env Html
hamletToRepHtml = withUrlRenderer
{-# DEPRECATED hamletToRepHtml "Use withUrlRenderer instead" #-}

-- | Deprecated synonym for 'withUrlRenderer'.
--
-- @since 1.2.0
giveUrlRenderer :: HasHandlerData env
                => ((Route (HandlerSite env) -> [(Text, Text)] -> Text) -> output)
                -> RIO env output
giveUrlRenderer = withUrlRenderer
{-# DEPRECATED giveUrlRenderer "Use withUrlRenderer instead" #-}

-- | Provide a URL rendering function to the given function and return the
-- result. Useful for processing Shakespearean templates.
--
-- @since 1.2.20
withUrlRenderer :: HasHandlerData env
                => ((Route (HandlerSite env) -> [(Text, Text)] -> Text) -> output)
                -> RIO env output
withUrlRenderer f = do
    render <- getUrlRenderParams
    return $ f render

-- | Get the request\'s 'W.Request' value.
waiRequest :: HasHandlerData env => RIO env W.Request
waiRequest = reqWaiRequest <$> getRequest

getMessageRender :: (HasHandlerData env, RenderMessage (HandlerSite env) message)
                 => RIO env (message -> Text)
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
cached :: (HasHandlerData env, Typeable a)
       => RIO env a
       -> RIO env a
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

-- | Retrieves a value from the cache used by 'cached'.
--
-- @since 1.6.10
cacheGet :: (HasHandlerData env, Typeable a)
         => RIO env (Maybe a)
cacheGet = do
  cache <- ghsCache <$> get
  pure $ Cache.cacheGet cache

-- | Sets a value in the cache used by 'cached'.
--
-- @since 1.6.10
cacheSet :: (HasHandlerData env, Typeable a)
         => a
         -> RIO env ()
cacheSet value = do
  gs <- get
  let cache = ghsCache gs
      newCache = Cache.cacheSet value cache
  put $ gs { ghsCache = newCache }

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
cachedBy :: (HasHandlerData env, Typeable a) => S.ByteString -> RIO env a -> RIO env a
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

-- | Retrieves a value from the cache used by 'cachedBy'.
--
-- @since 1.6.10
cacheByGet :: (HasHandlerData env, Typeable a)
           => S.ByteString
           -> RIO env (Maybe a)
cacheByGet key = do
  cache <- ghsCacheBy <$> get
  pure $ Cache.cacheByGet key cache

-- | Sets a value in the cache used by 'cachedBy'.
--
-- @since 1.6.10
cacheBySet :: (HasHandlerData env, Typeable a)
           => S.ByteString
           -> a
           -> RIO env ()
cacheBySet key value = do
  gs <- get
  let cache = ghsCacheBy gs
      newCache = Cache.cacheBySet key value cache
  put $ gs { ghsCacheBy = newCache }

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
languages :: HasHandlerData env => RIO env [Text]
languages = do
    mlang <- lookupSession langKey
    langs <- reqLangs <$> getRequest
    return $ maybe id (:) mlang langs

lookup' :: Eq a => a -> [(a, b)] -> [b]
lookup' a = map snd . filter (\x -> a == fst x)

-- | Lookup a request header.
--
-- @since 1.2.2
lookupHeader :: HasHandlerData env => CI S8.ByteString -> RIO env (Maybe S8.ByteString)
lookupHeader = fmap listToMaybe . lookupHeaders

-- | Lookup a request header.
--
-- @since 1.2.2
lookupHeaders :: HasHandlerData env => CI S8.ByteString -> RIO env [S8.ByteString]
lookupHeaders key = do
    req <- waiRequest
    return $ lookup' key $ W.requestHeaders req

-- | Lookup basic authentication data from __Authorization__ header of
-- request. Returns user name and password
--
-- @since 1.4.9
lookupBasicAuth :: (HasHandlerData env) => RIO env (Maybe (Text, Text))
lookupBasicAuth = fmap (>>= getBA) (lookupHeader "Authorization")
  where
    getBA bs = (decodeUtf8With lenientDecode *** decodeUtf8With lenientDecode)
               <$> extractBasicAuth bs

-- | Lookup bearer authentication datafrom __Authorization__ header of
-- request. Returns bearer token value
--
-- @since 1.4.9
lookupBearerAuth :: (HasHandlerData env) => RIO env (Maybe Text)
lookupBearerAuth = fmap (>>= getBR)
                   (lookupHeader "Authorization")
  where
    getBR bs = decodeUtf8With lenientDecode
               <$> extractBearerAuth bs


-- | Lookup for GET parameters.
lookupGetParams :: HasHandlerData env => Text -> RIO env [Text]
lookupGetParams pn = do
    rr <- getRequest
    return $ lookup' pn $ reqGetParams rr

-- | Lookup for GET parameters.
lookupGetParam :: HasHandlerData env => Text -> RIO env (Maybe Text)
lookupGetParam = fmap listToMaybe . lookupGetParams

-- | Lookup for POST parameters.
lookupPostParams :: HasHandlerData env => Text -> RIO env [Text]
lookupPostParams pn = do
    (pp, _) <- runRequestBody
    return $ lookup' pn pp

lookupPostParam :: HasHandlerData env
                => Text
                -> RIO env (Maybe Text)
lookupPostParam = fmap listToMaybe . lookupPostParams

-- | Lookup for POSTed files.
lookupFile :: HasHandlerData env
           => Text
           -> RIO env (Maybe FileInfo)
lookupFile = fmap listToMaybe . lookupFiles

-- | Lookup for POSTed files.
lookupFiles :: HasHandlerData env
            => Text
            -> RIO env [FileInfo]
lookupFiles pn = do
    (_, files) <- runRequestBody
    return $ lookup' pn files

-- | Lookup for cookie data.
lookupCookie :: HasHandlerData env => Text -> RIO env (Maybe Text)
lookupCookie = fmap listToMaybe . lookupCookies

-- | Lookup for cookie data.
lookupCookies :: HasHandlerData env => Text -> RIO env [Text]
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
selectRep :: HasHandlerData env
          => Writer.Writer (Endo [ProvidedRep (RIO env)]) ()
          -> RIO env TypedContent
selectRep w = do
    -- the content types are already sorted by q values
    -- which have been stripped
    cts <- fmap reqAccept getRequest

    case mapMaybe tryAccept cts of
        [] ->
            case reps of
                [] -> sendResponseStatus H.status500 ("No reps provided to selectRep" :: Text)
                rep:_ -> returnRep rep
        rep:_ -> returnRep rep
  where
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
                 then listToMaybe reps
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
rawRequestBody :: HasHandlerData env => ConduitT i S.ByteString (RIO env) ()
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
fileSource :: MonadResource m => FileInfo -> ConduitT () S.ByteString m ()
fileSource = transPipe liftResourceT . fileSourceRaw

-- | Extract a strict `ByteString` body from a `FileInfo`.
--
-- This function will block while reading the file.
--
-- > do
-- >     fileByteString <- fileSourceByteString fileInfo
--
-- @since 1.6.5
fileSourceByteString :: MonadResource m => FileInfo -> m S.ByteString
fileSourceByteString fileInfo = runConduit (L.toStrict <$> (fileSource fileInfo .| sinkLazy))

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
              -> ConduitT () (Flush Builder) (HandlerFor site) ()
              -> HandlerFor site TypedContent
respondSource ctype src = do
    hd <- ask
    -- Note that this implementation relies on the fact that the ResourceT
    -- environment provided by the server is the same one used in HandlerT.
    -- This is a safe assumption assuming the HandlerT is run correctly.
    return $ TypedContent ctype $ ContentSource
           $ transPipe (runRIO hd) src

-- | In a streaming response, send a single chunk of data. This function works
-- on most datatypes, such as @ByteString@ and @Html@.
--
-- @since 1.2.0
sendChunk :: Monad m => ToFlushBuilder a => a -> ConduitT i (Flush Builder) m ()
sendChunk = yield . toFlushBuilder

-- | In a streaming response, send a flush command, causing all buffered data
-- to be immediately sent to the client.
--
-- @since 1.2.0
sendFlush :: Monad m => ConduitT i (Flush Builder) m ()
sendFlush = yield Flush

-- | Type-specialized version of 'sendChunk' for strict @ByteString@s.
--
-- @since 1.2.0
sendChunkBS :: Monad m => S.ByteString -> ConduitT i (Flush Builder) m ()
sendChunkBS = sendChunk

-- | Type-specialized version of 'sendChunk' for lazy @ByteString@s.
--
-- @since 1.2.0
sendChunkLBS :: Monad m => L.ByteString -> ConduitT i (Flush Builder) m ()
sendChunkLBS = sendChunk

-- | Type-specialized version of 'sendChunk' for strict @Text@s.
--
-- @since 1.2.0
sendChunkText :: Monad m => T.Text -> ConduitT i (Flush Builder) m ()
sendChunkText = sendChunk

-- | Type-specialized version of 'sendChunk' for lazy @Text@s.
--
-- @since 1.2.0
sendChunkLazyText :: Monad m => TL.Text -> ConduitT i (Flush Builder) m ()
sendChunkLazyText = sendChunk

-- | Type-specialized version of 'sendChunk' for @Html@s.
--
-- @since 1.2.0
sendChunkHtml :: Monad m => Html -> ConduitT i (Flush Builder) m ()
sendChunkHtml = sendChunk

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
defaultCsrfCookieName :: IsString s => s
defaultCsrfCookieName = "XSRF-TOKEN"

-- | Sets a cookie with a CSRF token, using 'defaultCsrfCookieName' for the cookie name.
--
-- The cookie's path is set to @/@, making it valid for your whole website.
--
-- @since 1.4.14
setCsrfCookie :: HasHandlerData env => RIO env ()
setCsrfCookie = setCsrfCookieWithCookie defaultSetCookie
  { setCookieName = defaultCsrfCookieName
  , setCookiePath = Just "/"
  }

-- | Takes a 'SetCookie' and overrides its value with a CSRF token, then sets the cookie.
--
-- Make sure to set the 'setCookiePath' to the root path of your application, otherwise you'll generate a new CSRF token for every path of your app. If your app is run from from e.g. www.example.com\/app1, use @app1@. The vast majority of sites will just use @/@.
--
-- @since 1.4.14
setCsrfCookieWithCookie :: HasHandlerData env => SetCookie -> RIO env ()
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
checkCsrfHeaderNamed :: HasHandlerData env => CI S8.ByteString -> RIO env ()
checkCsrfHeaderNamed headerName = do
  (valid, mHeader) <- hasValidCsrfHeaderNamed' headerName
  unless valid (permissionDenied $ csrfErrorMessage [CSRFHeader (decodeUtf8 $ original headerName) mHeader])

-- | Takes a header name to lookup a CSRF token, and returns whether the value matches the token stored in the session.
--
-- @since 1.4.14
hasValidCsrfHeaderNamed :: HasHandlerData env => CI S8.ByteString -> RIO env Bool
hasValidCsrfHeaderNamed headerName = fst <$> hasValidCsrfHeaderNamed' headerName

-- | Like 'hasValidCsrfHeaderNamed', but also returns the header value to be used in error messages.
hasValidCsrfHeaderNamed' :: HasHandlerData env => CI S8.ByteString -> RIO env (Bool, Maybe Text)
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
checkCsrfParamNamed :: HasHandlerData env => Text -> RIO env ()
checkCsrfParamNamed paramName = do
  (valid, mParam) <- hasValidCsrfParamNamed' paramName
  unless valid (permissionDenied $ csrfErrorMessage [CSRFParam paramName mParam])

-- | Takes a POST parameter name to lookup a CSRF token, and returns whether the value matches the token stored in the session.
--
-- @since 1.4.14
hasValidCsrfParamNamed :: HasHandlerData env => Text -> RIO env Bool
hasValidCsrfParamNamed paramName = fst <$> hasValidCsrfParamNamed' paramName

-- | Like 'hasValidCsrfParamNamed', but also returns the param value to be used in error messages.
hasValidCsrfParamNamed' :: HasHandlerData env => Text -> RIO env (Bool, Maybe Text)
hasValidCsrfParamNamed' paramName = do
  mCsrfToken  <- reqToken <$> getRequest
  mCsrfParam <- lookupPostParam paramName

  return $ (validCsrf mCsrfToken (encodeUtf8 <$> mCsrfParam), mCsrfParam)

-- | Checks that a valid CSRF token is present in either the request headers or POST parameters.
-- If the value doesn't match the token stored in the session, this function throws a 'PermissionDenied' error.
--
-- @since 1.4.14
checkCsrfHeaderOrParam :: HasHandlerData env
                       => CI S8.ByteString -- ^ The header name to lookup the CSRF token
                       -> Text -- ^ The POST parameter name to lookup the CSRF token
                       -> RIO env ()
checkCsrfHeaderOrParam headerName paramName = do
  (validHeader, mHeader) <- hasValidCsrfHeaderNamed' headerName
  (validParam, mParam) <- hasValidCsrfParamNamed' paramName
  unless (validHeader || validParam) $ do
    let errorMessage = csrfErrorMessage $ [CSRFHeader (decodeUtf8 $ original headerName) mHeader, CSRFParam paramName mParam]
    logWarnS "yesod-core" errorMessage
    permissionDenied errorMessage

validCsrf :: Maybe Text -> Maybe S.ByteString -> Bool
-- It's important to use constant-time comparison (constEq) in order to avoid timing attacks.
validCsrf (Just token) (Just param) = encodeUtf8 token `constEq` param
validCsrf Nothing            _param = True
validCsrf (Just _token)     Nothing = False

data CSRFExpectation = CSRFHeader Text (Maybe Text) -- Key/Value
                     | CSRFParam Text (Maybe Text) -- Key/Value

csrfErrorMessage :: [CSRFExpectation]
                  -> Utf8Builder -- ^ Error message
csrfErrorMessage expectedLocations =
  "A valid CSRF token wasn't present. Because the request could have been forged, it's been rejected altogether.\n" <>
  "If you're a developer of this site, these tips will help you debug the issue:\n" <>
  "- Read the Yesod.Core.Handler docs of the yesod-core package for details on CSRF protection.\n" <>
  "- Check that your HTTP client is persisting cookies between requests, like a browser does.\n" <>
  "- By default, the CSRF token is sent to the client in a cookie named " <> defaultCsrfCookieName <> ".\n" <>
  "- The server is looking for the token in the following locations:\n" <>
  foldMap (\x -> csrfLocation x <> "\n") expectedLocations

  where csrfLocation expected = case expected of
          CSRFHeader k v -> "  - An HTTP header named " <> display k <> " " <> formatValue v
          CSRFParam k v -> "  - A POST parameter named " <> display k <> " " <> formatValue v

        formatValue :: Maybe Text -> Utf8Builder
        formatValue maybeText = case maybeText of
          Nothing -> "(which is not currently set)"
          Just t -> "(which has the current, incorrect value: '" <> display t <> "')"

getSubYesod :: HasHandlerData env => RIO env (SubHandlerSite env)
getSubYesod = view $ subHandlerDataL.to (rheChild . handlerEnv)

getRouteToParent :: HasHandlerData env => RIO env (Route (SubHandlerSite env) -> Route (HandlerSite env))
getRouteToParent = view $ subHandlerDataL.to (rheRouteToMaster . handlerEnv)

getSubCurrentRoute :: HasHandlerData env => RIO env (Maybe (Route (SubHandlerSite env)))
getSubCurrentRoute = view $ subHandlerDataL.to (rheRoute . handlerEnv)
