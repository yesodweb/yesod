{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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
module Yesod.Handler
    ( -- * Type families
      YesodSubRoute (..)
      -- * Handler monad
    , GHandler
      -- ** Read information from handler
    , getYesod
    , getYesodSub
    , getUrlRender
    , getUrlRenderParams
    , getCurrentRoute
    , getRouteToMaster
    , getRequest
    , waiRequest
    , runRequestBody
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
      -- **** Multi-lookup
    , lookupGetParams
    , lookupPostParams
    , lookupCookies
    , lookupFiles
      -- * Special responses
      -- ** Redirecting
    , RedirectUrl (..)
    , redirect
    , redirectWith
    , redirectToPost
      -- ** Errors
    , notFound
    , badMethod
    , permissionDenied
    , permissionDeniedI
    , invalidArgs
    , invalidArgsI
      -- ** Short-circuit responses.
    , sendFile
    , sendFilePart
    , sendResponse
    , sendResponseStatus
    , sendResponseCreated
    , sendWaiResponse
      -- * Setting headers
    , setCookie
    , getExpires
    , deleteCookie
    , setHeader
    , setLanguage
      -- ** Content caching and expiration
    , cacheSeconds
    , neverExpires
    , alreadyExpired
    , expiresAt
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
    , setMessage
    , setMessageI
    , getMessage
      -- * Helpers for specific content
      -- ** Hamlet
    , hamletToContent
    , hamletToRepHtml
      -- ** Misc
    , newIdent
      -- * Lifting
    , handlerToIO
      -- * i18n
    , getMessageRender
      -- * Per-request caching
    , cached
      -- * Internal Yesod
    , YesodApp
    , runSubsiteGetter
    , HandlerData
    , ErrorResponse (..)
    ) where

import           Data.Time                     (UTCTime, addUTCTime,
                                                getCurrentTime)
import           Yesod.Core.Internal.Request   (langKey, mkFileInfoFile,
                                                mkFileInfoLBS, mkFileInfoSource)

import           Control.Applicative           ((<$>))

import           Control.Monad                 (liftM)

import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Resource  (MonadResource, liftResourceT)

import qualified Network.HTTP.Types            as H
import qualified Network.Wai                   as W

import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error      (lenientDecode)
import qualified Data.Text.Lazy                as TL
import qualified Text.Blaze.Html.Renderer.Text as RenderText
import           Text.Hamlet                   (Html, HtmlUrl, hamlet)

import qualified Data.ByteString               as S
import qualified Data.Map                      as Map

import           Control.Arrow                 ((***))
import qualified Data.ByteString.Char8         as S8
import           Data.Maybe                    (mapMaybe)
import           Data.Monoid                   (Endo (..), mappend, mempty)
import           Data.Text                     (Text)
import qualified Network.Wai.Parse             as NWP
import           Text.Shakespeare.I18N         (RenderMessage (..))
import           Web.Cookie                    (SetCookie (..))
import           Yesod.Content                 (HasReps, chooseRep,
                                                formatRFC1123, toContent)

import           Text.Blaze.Html               (preEscapedToMarkup, toHtml)

import           Control.Monad.Trans.Resource  (ResourceT, runResourceT)
import           Data.Dynamic                  (fromDynamic, toDyn)
import qualified Data.IORef                    as I
import           Data.Maybe                    (listToMaybe)
import           Data.Typeable                 (Typeable, typeOf)
import           Yesod.Core.Handler.Class
import           Yesod.Core.Types
import           Yesod.Routes.Class            (Route)

class YesodSubRoute s y where
    fromSubRoute :: s -> y -> Route s -> Route y

get :: HandlerState m => m GHState
get = getGHState

put :: HandlerState m => GHState -> m ()
put = putGHState

modify :: HandlerState m => (GHState -> GHState) -> m ()
modify = stateGHState . (((), ) .)

tell :: HandlerState m => Endo [Header] -> m ()
tell hs = modify $ \g -> g { ghsHeaders = ghsHeaders g `mappend` hs }

hcError :: HandlerError m => ErrorResponse -> m a
hcError = handlerError . HCError

class SubsiteGetter g m s | g -> s where
  runSubsiteGetter :: g -> m s

instance (master ~ master'
         ) => SubsiteGetter (master -> sub) (GHandler anySub master') sub where
  runSubsiteGetter getter = getter <$> getYesod

instance (anySub ~ anySub'
         ,master ~ master'
         ) => SubsiteGetter (GHandler anySub master sub) (GHandler anySub' master') sub where
  runSubsiteGetter = id

getRequest :: HandlerReader m => m YesodRequest
getRequest = askYesodRequest

runRequestBody :: (MonadResource m, HandlerReader m, HandlerState m)
               => m RequestBodyContents
runRequestBody = do
    RunHandlerEnv {..} <- askHandlerEnv
    req <- askYesodRequest
    let len = W.requestBodyLength $ reqWaiRequest req
        upload = rheUpload len
    x <- get
    case ghsRBC x of
        Just rbc -> return rbc
        Nothing -> do
            rr <- waiRequest
            rbc <- liftResourceT $ rbHelper upload rr
            put x { ghsRBC = Just rbc }
            return rbc

rbHelper :: FileUpload -> W.Request -> ResourceT IO RequestBodyContents
rbHelper upload =
    case upload of
        FileUploadMemory s -> rbHelper' s mkFileInfoLBS
        FileUploadDisk s -> rbHelper' s mkFileInfoFile
        FileUploadSource s -> rbHelper' s mkFileInfoSource

rbHelper' :: NWP.BackEnd x
          -> (Text -> Text -> x -> FileInfo)
          -> W.Request
          -> ResourceT IO ([(Text, Text)], [(Text, FileInfo)])
rbHelper' backend mkFI req =
    (map fix1 *** mapMaybe fix2) <$> (NWP.parseRequestBody backend req)
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

-- | Get the sub application argument.
getYesodSub :: HandlerReader m => m (HandlerSub m)
getYesodSub = rheSub `liftM` askHandlerEnv

-- | Get the master site appliation argument.
getYesod :: HandlerReader m => m (HandlerMaster m)
getYesod = rheMaster `liftM` askHandlerEnv

-- | Get the URL rendering function.
getUrlRender :: HandlerReader m => m (Route (HandlerMaster m) -> Text)
getUrlRender = do
    x <- rheRender `liftM` askHandlerEnv
    return $ flip x []

-- | The URL rendering function with query-string parameters.
getUrlRenderParams
    :: HandlerReader m
    => m (Route (HandlerMaster m) -> [(Text, Text)] -> Text)
getUrlRenderParams = rheRender `liftM` askHandlerEnv

-- | Get the route requested by the user. If this is a 404 response- where the
-- user requested an invalid route- this function will return 'Nothing'.
getCurrentRoute :: HandlerReader m => m (Maybe (Route (HandlerSub m)))
getCurrentRoute = rheRoute `liftM` askHandlerEnv

-- | Get the function to promote a route for a subsite to a route for the
-- master site.
getRouteToMaster :: HandlerReader m => m (Route (HandlerSub m) -> Route (HandlerMaster m))
getRouteToMaster = rheToMaster `liftM` askHandlerEnv


-- | Returns a function that runs 'GHandler' actions inside @IO@.
--
-- Sometimes you want to run an inner 'GHandler' action outside
-- the control flow of an HTTP request (on the outer 'GHandler'
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
handlerToIO :: MonadIO m => GHandler sub master (GHandler sub master a -> m a)
handlerToIO =
  GHandler $ \oldHandlerData -> do
    -- Let go of the request body, cache and response headers.
    let oldReq    = handlerRequest oldHandlerData
        oldWaiReq = reqWaiRequest oldReq
        newWaiReq = oldWaiReq { W.requestBody = mempty
                              , W.requestBodyLength = W.KnownLength 0
                              }
        newReq    = oldReq { reqWaiRequest = newWaiReq }
        clearedOldHandlerData =
          oldHandlerData { handlerRequest = err "handlerRequest never here"
                         , handlerState   = err "handlerState never here" }
            where
              err :: String -> a
              err = error . ("handlerToIO: clearedOldHandlerData/" ++)
    newState <- liftIO $ do
      oldState <- I.readIORef (handlerState oldHandlerData)
      return $ oldState { ghsRBC = Nothing
                        , ghsIdent = 1
                        , ghsCache = mempty
                        , ghsHeaders = mempty }

    -- Return GHandler running function.
    return $ \(GHandler f) -> liftIO $ do
      -- The state IORef needs to be created here, otherwise it
      -- will be shared by different invocations of this function.
      newStateIORef <- I.newIORef newState
      runResourceT $ f clearedOldHandlerData
                         { handlerRequest = newReq
                         , handlerState   = newStateIORef }


-- | Redirect to the given route.
-- HTTP status code 303 for HTTP 1.1 clients and 302 for HTTP 1.0
-- This is the appropriate choice for a get-following-post
-- technique, which should be the usual use case.
--
-- If you want direct control of the final status code, or need a different
-- status code, please use 'redirectWith'.
redirect :: (HandlerError m, RedirectUrl (HandlerMaster m) url, HandlerReader m)
         => url -> m a
redirect url = do
    req <- waiRequest
    let status =
            if W.httpVersion req == H.http11
                then H.status303
                else H.status302
    redirectWith status url

-- | Redirect to the given URL with the specified status code.
redirectWith :: (HandlerError m, RedirectUrl (HandlerMaster m) url, HandlerReader m)
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
setUltDest :: (HandlerState m, RedirectUrl (HandlerMaster m) url)
           => url
           -> m ()
setUltDest url = do
    urlText <- toTextUrl url
    setSession ultDestKey urlText

-- | Same as 'setUltDest', but uses the current page.
--
-- If this is a 404 handler, there is no current page, and then this call does
-- nothing.
setUltDestCurrent :: HandlerState m => m ()
setUltDestCurrent = do
    route <- getCurrentRoute
    case route of
        Nothing -> return ()
        Just r -> do
            tm <- getRouteToMaster
            gets' <- reqGetParams `liftM` askYesodRequest
            setUltDest (tm r, gets')

-- | Sets the ultimate destination to the referer request header, if present.
--
-- This function will not overwrite an existing ultdest.
setUltDestReferer :: HandlerState m => m ()
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
redirectUltDest :: (RedirectUrl (HandlerMaster m) url, HandlerState m, HandlerError m)
                => url -- ^ default destination if nothing in session
                -> m a
redirectUltDest def = do
    mdest <- lookupSession ultDestKey
    deleteSession ultDestKey
    maybe (redirect def) redirect mdest

-- | Remove a previously set ultimate destination. See 'setUltDest'.
clearUltDest :: HandlerState m => m ()
clearUltDest = deleteSession ultDestKey

msgKey :: Text
msgKey = "_MSG"

-- | Sets a message in the user's session.
--
-- See 'getMessage'.
setMessage :: HandlerState m => Html -> m ()
setMessage = setSession msgKey . T.concat . TL.toChunks . RenderText.renderHtml

-- | Sets a message in the user's session.
--
-- See 'getMessage'.
setMessageI :: (HandlerState m, RenderMessage (HandlerMaster m) msg)
            => msg -> m ()
setMessageI msg = do
    mr <- getMessageRender
    setMessage $ toHtml $ mr msg

-- | Gets the message in the user's session, if available, and then clears the
-- variable.
--
-- See 'setMessage'.
getMessage :: HandlerState m => m (Maybe Html)
getMessage = do
    mmsg <- liftM (fmap preEscapedToMarkup) $ lookupSession msgKey
    deleteSession msgKey
    return mmsg

-- | Bypass remaining handler code and output the given file.
--
-- For some backends, this is more efficient than reading in the file to
-- memory, since they can optimize file sending via a system call to sendfile.
sendFile :: HandlerError m => ContentType -> FilePath -> m a
sendFile ct fp = handlerError $ HCSendFile ct fp Nothing

-- | Same as 'sendFile', but only sends part of a file.
sendFilePart :: HandlerError m
             => ContentType
             -> FilePath
             -> Integer -- ^ offset
             -> Integer -- ^ count
             -> m a
sendFilePart ct fp off count =
    handlerError $ HCSendFile ct fp $ Just $ W.FilePart off count

-- | Bypass remaining handler code and output the given content with a 200
-- status code.
sendResponse :: (HandlerError m, HasReps c) => c -> m a
sendResponse = handlerError . HCContent H.status200 . chooseRep

-- | Bypass remaining handler code and output the given content with the given
-- status code.
sendResponseStatus :: (HandlerError m, HasReps c) => H.Status -> c -> m a
sendResponseStatus s = handlerError . HCContent s . chooseRep

-- | Send a 201 "Created" response with the given route as the Location
-- response header.
sendResponseCreated :: HandlerError m => Route (HandlerMaster m) -> m a
sendResponseCreated url = do
    r <- getUrlRender
    handlerError $ HCCreated $ r url

-- | Send a 'W.Response'. Please note: this function is rarely
-- necessary, and will /disregard/ any changes to response headers and session
-- that you have already specified. This function short-circuits. It should be
-- considered only for very specific needs. If you are not sure if you need it,
-- you don't.
sendWaiResponse :: HandlerError m => W.Response -> m b
sendWaiResponse = handlerError . HCWai

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: HandlerError m => m a
notFound = hcError NotFound

-- | Return a 405 method not supported page.
badMethod :: HandlerError m => m a
badMethod = do
    w <- waiRequest
    hcError $ BadMethod $ W.requestMethod w

-- | Return a 403 permission denied page.
permissionDenied :: HandlerError m => Text -> m a
permissionDenied = hcError . PermissionDenied

-- | Return a 403 permission denied page.
permissionDeniedI :: (RenderMessage (HandlerMaster m) msg, HandlerError m)
                  => msg
                  -> m a
permissionDeniedI msg = do
    mr <- getMessageRender
    permissionDenied $ mr msg

-- | Return a 400 invalid arguments page.
invalidArgs :: HandlerError m => [Text] -> m a
invalidArgs = hcError . InvalidArgs

-- | Return a 400 invalid arguments page.
invalidArgsI :: (HandlerError m, RenderMessage (HandlerMaster m) msg) => [msg] -> m a
invalidArgsI msg = do
    mr <- getMessageRender
    invalidArgs $ map mr msg

------- Headers
-- | Set the cookie on the client.

setCookie :: HandlerState m => SetCookie -> m ()
setCookie = addHeader . AddCookie

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
deleteCookie :: HandlerState m
             => Text -- ^ key
             -> Text -- ^ path
             -> m ()
deleteCookie a = addHeader . DeleteCookie (encodeUtf8 a) . encodeUtf8


-- | Set the language in the user session. Will show up in 'languages' on the
-- next request.
setLanguage :: HandlerState m => Text -> m ()
setLanguage = setSession langKey

-- | Set an arbitrary response header.
--
-- Note that, while the data type used here is 'Text', you must provide only
-- ASCII value to be HTTP compliant.
setHeader :: HandlerState m => Text -> Text -> m ()
setHeader a = addHeader . Header (encodeUtf8 a) . encodeUtf8

-- | Set the Cache-Control header to indicate this response should be cached
-- for the given number of seconds.
cacheSeconds :: HandlerState m => Int -> m ()
cacheSeconds i = setHeader "Cache-Control" $ T.concat
    [ "max-age="
    , T.pack $ show i
    , ", public"
    ]

-- | Set the Expires header to some date in 2037. In other words, this content
-- is never (realistically) expired.
neverExpires :: HandlerState m => m ()
neverExpires = setHeader "Expires" "Thu, 31 Dec 2037 23:55:55 GMT"

-- | Set an Expires header in the past, meaning this content should not be
-- cached.
alreadyExpired :: HandlerState m => m ()
alreadyExpired = setHeader "Expires" "Thu, 01 Jan 1970 05:05:05 GMT"

-- | Set an Expires header to the given date.
expiresAt :: HandlerState m => UTCTime -> m ()
expiresAt = setHeader "Expires" . formatRFC1123

-- | Set a variable in the user's session.
--
-- The session is handled by the clientsession package: it sets an encrypted
-- and hashed cookie on the client. This ensures that all data is secure and
-- not tampered with.
setSession :: HandlerState m
           => Text -- ^ key
           -> Text -- ^ value
           -> m ()
setSession k = setSessionBS k . encodeUtf8

-- | Same as 'setSession', but uses binary data for the value.
setSessionBS :: HandlerState m
             => Text
             -> S.ByteString
             -> m ()
setSessionBS k = modify . modSession . Map.insert k

-- | Unsets a session variable. See 'setSession'.
deleteSession :: HandlerState m => Text -> m ()
deleteSession = modify . modSession . Map.delete

-- | Clear all session variables.
--
-- Since: 1.0.1
clearSession :: HandlerState m => m ()
clearSession = modify $ \x -> x { ghsSession = Map.empty }

modSession :: (SessionMap -> SessionMap) -> GHState -> GHState
modSession f x = x { ghsSession = f $ ghsSession x }

-- | Internal use only, not to be confused with 'setHeader'.
addHeader :: HandlerState m => Header -> m ()
addHeader = tell . Endo . (:)

-- | Some value which can be turned into a URL for redirects.
class RedirectUrl master a where
    -- | Converts the value to the URL and a list of query-string parameters.
    toTextUrl :: (HandlerReader m, HandlerMaster m ~ master) => a -> m Text

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

-- | Lookup for session data.
lookupSession :: HandlerState m => Text -> m (Maybe Text)
lookupSession = (liftM . fmap) (decodeUtf8With lenientDecode) . lookupSessionBS

-- | Lookup for session data in binary format.
lookupSessionBS :: HandlerState m => Text -> m (Maybe S.ByteString)
lookupSessionBS n = do
    m <- liftM ghsSession get
    return $ Map.lookup n m

-- | Get all session variables.
getSession :: HandlerState m => m SessionMap
getSession = liftM ghsSession get

-- | Get a unique identifier.
newIdent :: HandlerState m => m Text
newIdent = do
    x <- get
    let i' = ghsIdent x + 1
    put x { ghsIdent = i' }
    return $ T.pack $ 'h' : show i'

-- | Redirect to a POST resource.
--
-- This is not technically a redirect; instead, it returns an HTML page with a
-- POST form, and some Javascript to automatically submit the form. This can be
-- useful when you need to post a plain link somewhere that needs to cause
-- changes on the server.
redirectToPost :: (HandlerError m, RedirectUrl (HandlerMaster m) url)
               => url
               -> m a
redirectToPost url = do
    urlText <- toTextUrl url
    hamletToRepHtml [hamlet|
$newline never
$doctype 5

<html>
    <head>
        <title>Redirecting...
    <body onload="document.getElementById('form').submit()">
        <form id="form" method="post" action=#{urlText}>
            <noscript>
                <p>Javascript has been disabled; please click on the button below to be redirected.
            <input type="submit" value="Continue">
|] >>= sendResponse

-- | Converts the given Hamlet template into 'Content', which can be used in a
-- Yesod 'Response'.
hamletToContent :: HandlerReader m => HtmlUrl (Route (HandlerMaster m)) -> m Content
hamletToContent h = do
    render <- getUrlRenderParams
    return $ toContent $ h render

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
hamletToRepHtml :: HandlerReader m => HtmlUrl (Route (HandlerMaster m)) -> m RepHtml
hamletToRepHtml = liftM RepHtml . hamletToContent

-- | Get the request\'s 'W.Request' value.
waiRequest :: HandlerReader m => m W.Request
waiRequest = reqWaiRequest `liftM` getRequest

getMessageRender :: (HandlerReader m, RenderMessage (HandlerMaster m) message)
                 => m (message -> Text)
getMessageRender = do
    m <- getYesod
    l <- reqLangs `liftM` getRequest
    return $ renderMessage m l

-- | Use a per-request cache to avoid performing the same action multiple
-- times. Note that values are stored by their type. Therefore, you should use
-- newtype wrappers to distinguish logically different types.
--
-- Since 1.2.0
cached :: (HandlerState m, Typeable a)
       => m a
       -> m a
cached f = do
    gs <- get
    let cache = ghsCache gs
    case clookup cache of
        Just val -> return val
        Nothing -> do
            val <- f
            put $ gs { ghsCache = cinsert val cache }
            return val
  where
    clookup :: Typeable a => Cache -> Maybe a
    clookup (Cache m) =
        res
      where
        res = Map.lookup (typeOf $ fromJust res) m >>= fromDynamic
        fromJust :: Maybe a -> a
        fromJust = error "Yesod.Handler.cached.fromJust: Argument to typeOf was evaluated"

    cinsert :: Typeable a => a -> Cache -> Cache
    cinsert v (Cache m) = Cache (Map.insert (typeOf v) (toDyn v) m)

-- | Get the list of supported languages supplied by the user.
--
-- Languages are determined based on the following three (in descending order
-- of preference):
--
-- * The _LANG get parameter.
--
-- * The _LANG cookie.
--
-- * The _LANG user session variable.
--
-- * Accept-Language HTTP header.
--
-- Yesod will seek the first language from the returned list matched with languages supporting by your application. This language will be used to render i18n templates.
-- If a matching language is not found the default language will be used.
--
-- This is handled by parseWaiRequest (not exposed).
languages :: HandlerReader m => m [Text]
languages = reqLangs `liftM` getRequest

lookup' :: Eq a => a -> [(a, b)] -> [b]
lookup' a = map snd . filter (\x -> a == fst x)

-- | Lookup for GET parameters.
lookupGetParams :: HandlerReader m => Text -> m [Text]
lookupGetParams pn = do
    rr <- getRequest
    return $ lookup' pn $ reqGetParams rr

-- | Lookup for GET parameters.
lookupGetParam :: HandlerReader m => Text -> m (Maybe Text)
lookupGetParam = liftM listToMaybe . lookupGetParams

-- | Lookup for POST parameters.
lookupPostParams :: (MonadResource m, HandlerState m) => Text -> m [Text]
lookupPostParams pn = do
    (pp, _) <- runRequestBody
    return $ lookup' pn pp

lookupPostParam :: (MonadResource m, HandlerState m)
                => Text
                -> m (Maybe Text)
lookupPostParam = liftM listToMaybe . lookupPostParams

-- | Lookup for POSTed files.
lookupFile :: (HandlerState m, MonadResource m)
           => Text
           -> m (Maybe FileInfo)
lookupFile = liftM listToMaybe . lookupFiles

-- | Lookup for POSTed files.
lookupFiles :: (HandlerState m, MonadResource m)
            => Text
            -> m [FileInfo]
lookupFiles pn = do
    (_, files) <- runRequestBody
    return $ lookup' pn files

-- | Lookup for cookie data.
lookupCookie :: HandlerReader m => Text -> m (Maybe Text)
lookupCookie = liftM listToMaybe . lookupCookies

-- | Lookup for cookie data.
lookupCookies :: HandlerReader m => Text -> m [Text]
lookupCookies pn = do
    rr <- getRequest
    return $ lookup' pn $ reqCookies rr
