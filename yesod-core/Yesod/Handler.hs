{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
---------------------------------------------------------
--
-- Module        : Yesod.Handler
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : unstable
-- Portability   : portable
--
-- Define Handler stuff.
--
---------------------------------------------------------
module Yesod.Handler
    ( -- * Type families
      Route
    , YesodSubRoute (..)
      -- * Handler monad
    , GHandler
    , GGHandler
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
      -- * Special responses
      -- ** Redirecting
    , RedirectType (..)
    , redirect
    , redirectParams
    , redirectString
    , redirectText
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
    , getSession
    , setSession
    , deleteSession
      -- ** Ultimate destination
    , setUltDest
    , setUltDestString
    , setUltDestText
    , setUltDest'
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
    , liftIOHandler
      -- * i18n
    , getMessageRender
      -- * Internal Yesod
    , runHandler
    , YesodApp (..)
    , runSubsiteGetter
    , toMasterHandler
    , toMasterHandlerDyn
    , toMasterHandlerMaybe
    , localNoCurrent
    , HandlerData
    , ErrorResponse (..)
    , YesodAppResult (..)
    , handlerToYAR
    , yarToResponse
    , headerToPair
    ) where

import Prelude hiding (catch)
import Yesod.Internal.Request
import Yesod.Internal
import Data.Time (UTCTime)

import Control.Exception hiding (Handler, catch, finally)
import Control.Applicative

import Control.Monad (liftM, join, MonadPlus)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Error (throwError, ErrorT (..), Error (..))

import System.IO
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import Control.Failure (Failure (failure))

import Text.Hamlet
import qualified Text.Blaze.Renderer.Text
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL

import Control.Monad.IO.Control (MonadControlIO)
import Control.Monad.Trans.Control (MonadTransControl, liftControl)
import qualified Data.Map as Map
import qualified Data.ByteString as S
import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee (..), run_, ($$))
import Network.Wai.Parse (parseHttpAccept)

import Yesod.Content
import Data.Maybe (fromMaybe)
import Web.Cookie (SetCookie (..), renderSetCookie)
import Control.Arrow (second, (***))
import qualified Network.Wai.Parse as NWP
import Data.Monoid (mappend, mempty, Endo (..))
import qualified Data.ByteString.Char8 as S8
import Data.CaseInsensitive (CI)
import Blaze.ByteString.Builder (toByteString)
import Data.Text (Text)
import Yesod.Message (RenderMessage (..))

import Text.Blaze (toHtml, preEscapedText)
import Yesod.Internal.TestApi (catchIter)

-- | The type-safe URLs associated with a site argument.
type family Route a

class YesodSubRoute s y where
    fromSubRoute :: s -> y -> Route s -> Route y

data HandlerData sub master = HandlerData
    { handlerRequest  :: Request
    , handlerSub      :: sub
    , handlerMaster   :: master
    , handlerRoute    :: Maybe (Route sub)
    , handlerRender   :: Route master -> [(Text, Text)] -> Text
    , handlerToMaster :: Route sub -> Route master
    }

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
    { handlerSub = ts $ handlerMaster hd
    , handlerToMaster = tm
    , handlerRoute = route
    }

-- | Used internally for promoting subsite handler functions to master site
-- handler functions. Should not be needed by users.
toMasterHandler :: (Route sub -> Route master)
                -> (master -> sub)
                -> Route sub
                -> GGHandler sub master mo a
                -> GGHandler sub' master mo a
toMasterHandler tm ts route (GHandler h) =
    GHandler $ withReaderT (handlerSubData tm ts route) h

toMasterHandlerDyn :: Monad mo
                   => (Route sub -> Route master)
                   -> GGHandler sub' master mo sub
                   -> Route sub
                   -> GGHandler sub master mo a
                   -> GGHandler sub' master mo a
toMasterHandlerDyn tm getSub route (GHandler h) = do
    sub <- getSub
    GHandler $ withReaderT (handlerSubData tm (const sub) route) h

class SubsiteGetter g m s | g -> s where
  runSubsiteGetter :: g -> m s

instance (master ~ master'
         ) => SubsiteGetter (master -> sub) (GHandler anySub master') sub where
  runSubsiteGetter getter = getter <$> getYesod

instance (anySub ~ anySub'
         ,master ~ master'
         ) => SubsiteGetter (GHandler anySub master sub) (GHandler anySub' master') sub where
  runSubsiteGetter = id

toMasterHandlerMaybe :: (Route sub -> Route master)
                     -> (master -> sub)
                     -> Maybe (Route sub)
                     -> GGHandler sub master mo a
                     -> GGHandler sub' master mo a
toMasterHandlerMaybe tm ts route (GHandler h) =
    GHandler $ withReaderT (handlerSubDataMaybe tm ts route) h

-- | A generic handler monad, which can have a different subsite and master
-- site. This monad is a combination of 'ReaderT' for basic arguments, a
-- 'WriterT' for headers and session, and an 'MEitherT' monad for handling
-- special responses. It is declared as a newtype to make compiler errors more
-- readable.
newtype GGHandler sub master m a =
    GHandler
        { unGHandler :: GHInner sub master m a
        }
    deriving (Functor, Applicative, Monad, MonadIO, MonadControlIO, MonadPlus)

instance MonadTrans (GGHandler s m) where
    lift = GHandler . lift . lift . lift . lift

type GHandler sub master = GGHandler sub master (Iteratee ByteString IO)

data GHState = GHState
    { ghsSession :: SessionMap
    , ghsRBC :: Maybe RequestBodyContents
    , ghsIdent :: Int
    }

type GHInner s m monad = -- FIXME collapse the stack
    ReaderT (HandlerData s m) (
    ErrorT HandlerContents (
    WriterT (Endo [Header]) (
    StateT GHState (
    monad
    ))))

type SessionMap = Map.Map Text Text

-- | An extension of the basic WAI 'W.Application' datatype to provide extra
-- features needed by Yesod. Users should never need to use this directly, as
-- the 'GHandler' monad and template haskell code should hide it away.
newtype YesodApp = YesodApp
    { unYesodApp
    :: (ErrorResponse -> YesodApp)
    -> Request
    -> [ContentType]
    -> SessionMap
    -> Iteratee ByteString IO YesodAppResult
    }

data YesodAppResult
    = YARWai W.Response
    | YARPlain H.Status [Header] ContentType Content SessionMap

data HandlerContents =
      HCContent H.Status ChooseRep
    | HCError ErrorResponse
    | HCSendFile ContentType FilePath (Maybe W.FilePart) -- FIXME replace FilePath with opaque type from system-filepath?
    | HCRedirect RedirectType Text
    | HCCreated Text
    | HCWai W.Response

instance Error HandlerContents where
    strMsg = HCError . InternalError . T.pack

getRequest :: Monad mo => GGHandler s m mo Request
getRequest = handlerRequest `liftM` GHandler ask

instance Monad monad => Failure ErrorResponse (GGHandler sub master monad) where
    failure = GHandler . lift . throwError . HCError

runRequestBody :: GHandler s m RequestBodyContents
runRequestBody = do
    x <- GHandler $ lift $ lift $ lift get
    case ghsRBC x of
        Just rbc -> return rbc
        Nothing -> do
            rr <- waiRequest
            rbc <- lift $ rbHelper rr
            GHandler $ lift $ lift $ lift $ put x { ghsRBC = Just rbc }
            return rbc

rbHelper :: W.Request -> Iteratee ByteString IO RequestBodyContents
rbHelper req =
    (map fix1 *** map fix2) <$> iter
  where
    iter = NWP.parseRequestBody NWP.lbsSink req
    fix1 = go *** go
    fix2 (x, NWP.FileInfo a b c) =
        (go x, FileInfo (go a) (go b) c)
    go = decodeUtf8With lenientDecode

-- | Get the sub application argument.
getYesodSub :: Monad m => GGHandler sub master m sub
getYesodSub = handlerSub `liftM` GHandler ask

-- | Get the master site appliation argument.
getYesod :: Monad m => GGHandler sub master m master
getYesod = handlerMaster `liftM` GHandler ask

-- | Get the URL rendering function.
getUrlRender :: Monad m => GGHandler sub master m (Route master -> Text)
getUrlRender = do
    x <- handlerRender `liftM` GHandler ask
    return $ flip x []

-- | The URL rendering function with query-string parameters.
getUrlRenderParams
    :: Monad m
    => GGHandler sub master m (Route master -> [(Text, Text)] -> Text)
getUrlRenderParams = handlerRender `liftM` GHandler ask

-- | Get the route requested by the user. If this is a 404 response- where the
-- user requested an invalid route- this function will return 'Nothing'.
getCurrentRoute :: Monad m => GGHandler sub master m (Maybe (Route sub))
getCurrentRoute = handlerRoute `liftM` GHandler ask

-- | Get the function to promote a route for a subsite to a route for the
-- master site.
getRouteToMaster :: Monad m => GGHandler sub master m (Route sub -> Route master)
getRouteToMaster = handlerToMaster `liftM` GHandler ask

-- | Function used internally by Yesod in the process of converting a
-- 'GHandler' into an 'W.Application'. Should not be needed by users.
runHandler :: HasReps c
           => GHandler sub master c
           -> (Route master -> [(Text, Text)] -> Text)
           -> Maybe (Route sub)
           -> (Route sub -> Route master)
           -> master
           -> sub
           -> YesodApp
runHandler handler mrender sroute tomr ma sa =
  YesodApp $ \eh rr cts initSession -> do
    let toErrorHandler e =
            case fromException e of
                Just x -> x
                Nothing -> InternalError $ T.pack $ show e
    let hd = HandlerData
            { handlerRequest = rr
            , handlerSub = sa
            , handlerMaster = ma
            , handlerRoute = sroute
            , handlerRender = mrender
            , handlerToMaster = tomr
            }
    let initSession' = GHState initSession Nothing 1
    ((contents', headers), finalSession) <- catchIter (
        fmap (second ghsSession)
      $ flip runStateT initSession'
      $ runWriterT
      $ runErrorT
      $ flip runReaderT hd
      $ unGHandler handler
        ) (\e -> return ((Left $ HCError $ toErrorHandler e, mempty), initSession))
    let contents = either id (HCContent H.status200 . chooseRep) contents'
    let handleError e = do
            yar <- unYesodApp (eh e) safeEh rr cts finalSession
            case yar of
                YARPlain _ hs ct c sess ->
                    let hs' = appEndo headers hs
                     in return $ YARPlain (getStatus e) hs' ct c sess
                YARWai _ -> return yar
    let sendFile' ct fp p =
            return $ YARPlain H.status200 (appEndo headers []) ct (ContentFile fp p) finalSession
    case contents of
        HCContent status a -> do
            (ct, c) <- liftIO $ a cts
            return $ YARPlain status (appEndo headers []) ct c finalSession
        HCError e -> handleError e
        HCRedirect rt loc -> do
            let hs = Header "Location" (encodeUtf8 loc) : appEndo headers []
            return $ YARPlain
                (getRedirectStatus rt) hs typePlain emptyContent
                finalSession
        HCSendFile ct fp p -> catchIter
            (sendFile' ct fp p)
            (handleError . toErrorHandler)
        HCCreated loc -> do
            let hs = Header "Location" (encodeUtf8 loc) : appEndo headers []
            return $ YARPlain
                H.status201
                hs
                typePlain
                emptyContent
                finalSession
        HCWai r -> return $ YARWai r

safeEh :: ErrorResponse -> YesodApp
safeEh er = YesodApp $ \_ _ _ session -> do
    liftIO $ hPutStrLn stderr $ "Error handler errored out: " ++ show er
    return $ YARPlain
        H.status500
        []
        typePlain
        (toContent ("Internal Server Error" :: S.ByteString))
        session

-- | Redirect to the given route.
redirect :: Monad mo => RedirectType -> Route master -> GGHandler sub master mo a
redirect rt url = redirectParams rt url []

-- | Redirects to the given route with the associated query-string parameters.
redirectParams :: Monad mo
               => RedirectType -> Route master -> [(Text, Text)]
               -> GGHandler sub master mo a
redirectParams rt url params = do
    r <- getUrlRenderParams
    redirectString rt $ r url params

-- | Redirect to the given URL.
redirectString, redirectText :: Monad mo => RedirectType -> Text -> GGHandler sub master mo a
redirectText rt = GHandler . lift . throwError . HCRedirect rt
redirectString = redirectText
{-# DEPRECATED redirectString "Use redirectText instead" #-}

ultDestKey :: Text
ultDestKey = "_ULT"

-- | Sets the ultimate destination variable to the given route.
--
-- An ultimate destination is stored in the user session and can be loaded
-- later by 'redirectUltDest'.
setUltDest :: Monad mo => Route master -> GGHandler sub master mo ()
setUltDest dest = do
    render <- getUrlRender
    setUltDestString $ render dest

-- | Same as 'setUltDest', but use the given string.
setUltDestText :: Monad mo => Text -> GGHandler sub master mo ()
setUltDestText = setSession ultDestKey

setUltDestString :: Monad mo => Text -> GGHandler sub master mo ()
setUltDestString = setSession ultDestKey
{-# DEPRECATED setUltDestString "Use setUltDestText instead" #-}

-- | Same as 'setUltDest', but uses the current page.
--
-- If this is a 404 handler, there is no current page, and then this call does
-- nothing.
setUltDest' :: Monad mo => GGHandler sub master mo ()
setUltDest' = do
    route <- getCurrentRoute
    case route of
        Nothing -> return ()
        Just r -> do
            tm <- getRouteToMaster
            gets' <- reqGetParams `liftM` handlerRequest `liftM` GHandler ask
            render <- getUrlRenderParams
            setUltDestString $ render (tm r) gets'

-- | Sets the ultimate destination to the referer request header, if present.
--
-- This function will not overwrite an existing ultdest.
setUltDestReferer :: Monad mo => GGHandler sub master mo ()
setUltDestReferer = do
    mdest <- lookupSession ultDestKey
    maybe
        (waiRequest >>= maybe (return ()) setUltDestBS . lookup "referer" . W.requestHeaders)
        (const $ return ())
        mdest
  where
    setUltDestBS = setUltDestText . T.pack . S8.unpack

-- | Redirect to the ultimate destination in the user's session. Clear the
-- value from the session.
--
-- The ultimate destination is set with 'setUltDest'.
redirectUltDest :: Monad mo
                => RedirectType
                -> Route master -- ^ default destination if nothing in session
                -> GGHandler sub master mo a
redirectUltDest rt def = do
    mdest <- lookupSession ultDestKey
    deleteSession ultDestKey
    maybe (redirect rt def) (redirectText rt) mdest

-- | Remove a previously set ultimate destination. See 'setUltDest'.
clearUltDest :: Monad mo => GGHandler sub master mo ()
clearUltDest = deleteSession ultDestKey

msgKey :: Text
msgKey = "_MSG"

-- | Sets a message in the user's session.
--
-- See 'getMessage'.
setMessage :: Monad mo => Html -> GGHandler sub master mo ()
setMessage = setSession msgKey . T.concat . TL.toChunks . Text.Blaze.Renderer.Text.renderHtml

-- | Sets a message in the user's session.
--
-- See 'getMessage'.
setMessageI :: (RenderMessage y msg, Monad mo) => msg -> GGHandler sub y mo ()
setMessageI msg = do
    mr <- getMessageRender
    setMessage $ toHtml $ mr msg

-- | Gets the message in the user's session, if available, and then clears the
-- variable.
--
-- See 'setMessage'.
getMessage :: Monad mo => GGHandler sub master mo (Maybe Html)
getMessage = do
    mmsg <- liftM (fmap preEscapedText) $ lookupSession msgKey
    deleteSession msgKey
    return mmsg

-- | Bypass remaining handler code and output the given file.
--
-- For some backends, this is more efficient than reading in the file to
-- memory, since they can optimize file sending via a system call to sendfile.
sendFile :: Monad mo => ContentType -> FilePath -> GGHandler sub master mo a
sendFile ct fp = GHandler . lift . throwError $ HCSendFile ct fp Nothing

-- | Same as 'sendFile', but only sends part of a file.
sendFilePart :: Monad mo
             => ContentType
             -> FilePath
             -> Integer -- ^ offset
             -> Integer -- ^ count
             -> GGHandler sub master mo a
sendFilePart ct fp off count =
    GHandler . lift . throwError $ HCSendFile ct fp $ Just $ W.FilePart off count

-- | Bypass remaining handler code and output the given content with a 200
-- status code.
sendResponse :: (Monad mo, HasReps c) => c -> GGHandler sub master mo a
sendResponse = GHandler . lift . throwError . HCContent H.status200
             . chooseRep

-- | Bypass remaining handler code and output the given content with the given
-- status code.
sendResponseStatus :: (Monad mo, HasReps c) => H.Status -> c -> GGHandler s m mo a
sendResponseStatus s = GHandler . lift . throwError . HCContent s
                     . chooseRep

-- | Send a 201 "Created" response with the given route as the Location
-- response header.
sendResponseCreated :: Monad mo => Route m -> GGHandler s m mo a
sendResponseCreated url = do
    r <- getUrlRender
    GHandler $ lift $ throwError $ HCCreated $ r url

-- | Send a 'W.Response'. Please note: this function is rarely
-- necessary, and will /disregard/ any changes to response headers and session
-- that you have already specified. This function short-circuits. It should be
-- considered only for very specific needs. If you are not sure if you need it,
-- you don't.
sendWaiResponse :: Monad mo => W.Response -> GGHandler s m mo b
sendWaiResponse = GHandler . lift . throwError . HCWai

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: Failure ErrorResponse m => m a
notFound = failure NotFound

-- | Return a 405 method not supported page.
badMethod :: Monad mo => GGHandler s m mo a
badMethod = do
    w <- waiRequest
    failure $ BadMethod $ W.requestMethod w

-- | Return a 403 permission denied page.
permissionDenied :: Failure ErrorResponse m => Text -> m a
permissionDenied = failure . PermissionDenied

-- | Return a 403 permission denied page.
permissionDeniedI :: (RenderMessage y msg, Monad mo) => msg -> GGHandler s y mo a
permissionDeniedI msg = do
    mr <- getMessageRender
    permissionDenied $ mr msg

-- | Return a 400 invalid arguments page.
invalidArgs :: Failure ErrorResponse m => [Text] -> m a
invalidArgs = failure . InvalidArgs

-- | Return a 400 invalid arguments page.
invalidArgsI :: (RenderMessage y msg, Monad mo) => [msg] -> GGHandler s y mo a
invalidArgsI msg = do
    mr <- getMessageRender
    invalidArgs $ map mr msg

------- Headers
-- | Set the cookie on the client.
setCookie :: Monad mo
          => Int -- ^ minutes to timeout
          -> H.Ascii -- ^ key
          -> H.Ascii -- ^ value
          -> GGHandler sub master mo ()
setCookie a b = addHeader . AddCookie a b

-- | Unset the cookie on the client.
deleteCookie :: Monad mo => H.Ascii -> GGHandler sub master mo ()
deleteCookie = addHeader . DeleteCookie

-- | Set the language in the user session. Will show up in 'languages' on the
-- next request.
setLanguage :: Monad mo => Text -> GGHandler sub master mo ()
setLanguage = setSession langKey

-- | Set an arbitrary response header.
setHeader :: Monad mo
          => CI H.Ascii -> H.Ascii -> GGHandler sub master mo ()
setHeader a = addHeader . Header a

-- | Set the Cache-Control header to indicate this response should be cached
-- for the given number of seconds.
cacheSeconds :: Monad mo => Int -> GGHandler s m mo ()
cacheSeconds i = setHeader "Cache-Control" $ S8.pack $ concat
    [ "max-age="
    , show i
    , ", public"
    ]

-- | Set the Expires header to some date in 2037. In other words, this content
-- is never (realistically) expired.
neverExpires :: Monad mo => GGHandler s m mo ()
neverExpires = setHeader "Expires" "Thu, 31 Dec 2037 23:55:55 GMT"

-- | Set an Expires header in the past, meaning this content should not be
-- cached.
alreadyExpired :: Monad mo => GGHandler s m mo ()
alreadyExpired = setHeader "Expires" "Thu, 01 Jan 1970 05:05:05 GMT"

-- | Set an Expires header to the given date.
expiresAt :: Monad mo => UTCTime -> GGHandler s m mo ()
expiresAt = setHeader "Expires" . encodeUtf8 . formatRFC1123

-- | Set a variable in the user's session.
--
-- The session is handled by the clientsession package: it sets an encrypted
-- and hashed cookie on the client. This ensures that all data is secure and
-- not tampered with.
setSession :: Monad mo
           => Text -- ^ key
           -> Text -- ^ value
           -> GGHandler sub master mo ()
setSession k = GHandler . lift . lift . lift . modify . modSession . Map.insert k

-- | Unsets a session variable. See 'setSession'.
deleteSession :: Monad mo => Text -> GGHandler sub master mo ()
deleteSession = GHandler . lift . lift . lift . modify . modSession . Map.delete

modSession :: (SessionMap -> SessionMap) -> GHState -> GHState
modSession f x = x { ghsSession = f $ ghsSession x }

-- | Internal use only, not to be confused with 'setHeader'.
addHeader :: Monad mo => Header -> GGHandler sub master mo ()
addHeader = GHandler . lift . lift . tell . Endo . (:)

getStatus :: ErrorResponse -> H.Status
getStatus NotFound = H.status404
getStatus (InternalError _) = H.status500
getStatus (InvalidArgs _) = H.status400
getStatus (PermissionDenied _) = H.status403
getStatus (BadMethod _) = H.status405

getRedirectStatus :: RedirectType -> H.Status
getRedirectStatus RedirectPermanent = H.status301
getRedirectStatus RedirectTemporary = H.status302
getRedirectStatus RedirectSeeOther = H.status303

-- | Different types of redirects.
data RedirectType = RedirectPermanent
                  | RedirectTemporary
                  | RedirectSeeOther
    deriving (Show, Eq)

localNoCurrent :: Monad mo => GGHandler s m mo a -> GGHandler s m mo a
localNoCurrent =
    GHandler . local (\hd -> hd { handlerRoute = Nothing }) . unGHandler

-- | Lookup for session data.
lookupSession :: Monad mo => Text -> GGHandler s m mo (Maybe Text)
lookupSession n = GHandler $ do
    m <- liftM ghsSession $ lift $ lift $ lift get
    return $ Map.lookup n m

-- | Get all session variables.
getSession :: Monad mo => GGHandler s m mo SessionMap
getSession = liftM ghsSession $ GHandler $ lift $ lift $ lift get

handlerToYAR :: (HasReps a, HasReps b)
             => m -- ^ master site foundation
             -> s -- ^ sub site foundation
             -> (Route s -> Route m)
             -> (Route m -> [(Text, Text)] -> Text)
             -> (ErrorResponse -> GHandler s m a)
             -> Request
             -> Maybe (Route s)
             -> SessionMap
             -> GHandler s m b
             -> Iteratee ByteString IO YesodAppResult
handlerToYAR y s toMasterRoute render errorHandler rr murl sessionMap h =
    unYesodApp ya eh' rr types sessionMap
  where
    ya = runHandler h render murl toMasterRoute y s
    eh' er = runHandler (errorHandler' er) render murl toMasterRoute y s
    types = httpAccept $ reqWaiRequest rr
    errorHandler' = localNoCurrent . errorHandler

type HeaderRenderer = [Header]
                   -> ContentType
                   -> SessionMap
                   -> [(CI H.Ascii, H.Ascii)]

yarToResponse :: HeaderRenderer -> YesodAppResult -> W.Response
yarToResponse _ (YARWai a) = a
yarToResponse renderHeaders (YARPlain s hs ct c sessionFinal) =
    case c of
        ContentBuilder b mlen ->
            let hs' = maybe finalHeaders finalHeaders' mlen
             in W.ResponseBuilder s hs' b
        ContentFile fp p -> W.ResponseFile s finalHeaders fp p
        ContentEnum e ->
            W.ResponseEnumerator $ \iter -> run_ $ e $$ iter s finalHeaders
  where
    finalHeaders = renderHeaders hs ct sessionFinal
    finalHeaders' len = ("Content-Length", S8.pack $ show len)
                      : finalHeaders
    {-
    getExpires m = fromIntegral (m * 60) `addUTCTime` now
    sessionVal =
        case key' of
            Nothing -> B.empty
            Just key'' -> encodeSession key'' exp' host
                        $ Map.toList
                        $ Map.insert nonceKey (reqNonce rr) sessionFinal
    hs' =
            case key' of
                Nothing -> hs
                Just _ -> AddCookie
                        (clientSessionDuration y)
                        sessionName
                        (bsToChars sessionVal)
                      : hs
    hs'' = map (headerToPair getExpires) hs'
    hs''' = ("Content-Type", charsToBs ct) : hs''
    -}

httpAccept :: W.Request -> [ContentType]
httpAccept = parseHttpAccept
           . fromMaybe mempty
           . lookup "Accept"
           . W.requestHeaders

-- | Convert Header to a key/value pair.
headerToPair :: S.ByteString -- ^ cookie path
             -> (Int -> UTCTime) -- ^ minutes -> expiration time
             -> Header
             -> (CI H.Ascii, H.Ascii)
headerToPair cp getExpires (AddCookie minutes key value) =
    ("Set-Cookie", toByteString $ renderSetCookie $ SetCookie
        { setCookieName = key
        , setCookieValue = value
        , setCookiePath = Just cp
        , setCookieExpires =
            if minutes == 0
                then Nothing
                else Just $ getExpires minutes
        , setCookieDomain = Nothing
        , setCookieHttpOnly = True
        })
headerToPair cp _ (DeleteCookie key) =
    ( "Set-Cookie"
    , key `mappend` "=; path=" `mappend` cp `mappend` "; expires=Thu, 01-Jan-1970 00:00:00 GMT"
    )
headerToPair _ _ (Header key value) = (key, value)

-- | Get a unique identifier.
newIdent :: Monad mo => GGHandler sub master mo String -- FIXME use Text
newIdent = GHandler $ lift $ lift $ lift $ do
    x <- get
    let i' = ghsIdent x + 1
    put x { ghsIdent = i' }
    return $ 'h' : show i'

liftIOHandler :: MonadIO mo
              => GGHandler sub master IO a
              -> GGHandler sub master mo a
liftIOHandler m = GHandler $
                   ReaderT $ \r ->
                     ErrorT $
                       WriterT $
                         StateT $ \s ->
                           liftIO $ runGGHandler m r s

runGGHandler :: GGHandler sub master m a
            -> HandlerData sub master
            -> GHState
            -> m ( ( Either HandlerContents a
                   , Endo [Header]
                   )
                 , GHState
                 )
runGGHandler m r s = runStateT
                      (runWriterT
                        (runErrorT
                          (runReaderT
                            (unGHandler m) r))) s

instance MonadTransControl (GGHandler s m) where
    liftControl f =
      GHandler $
        liftControl $ \runRdr ->
          liftControl $ \runErr ->
            liftControl $ \runWrt ->
              liftControl $ \runSt ->
                f ( liftM ( GHandler
                          . join . lift
                          . join . lift
                          . join . lift
                          )
                  . runSt . runWrt . runErr . runRdr
                  . unGHandler
                  )

-- | Redirect to a POST resource.
--
-- This is not technically a redirect; instead, it returns an HTML page with a
-- POST form, and some Javascript to automatically submit the form. This can be
-- useful when you need to post a plain link somewhere that needs to cause
-- changes on the server.
redirectToPost :: Monad mo => Route master -> GGHandler sub master mo a
redirectToPost dest = hamletToRepHtml
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
\<!DOCTYPE html>

<html>
    <head>
        <title>Redirecting...
    <body onload="document.getElementById('form').submit()">
        <form id="form" method="post" action="@{dest}">
            <noscript>
                <p>Javascript has been disabled; please click on the button below to be redirected.
            <input type="submit" value="Continue">
|] >>= sendResponse

-- | Converts the given Hamlet template into 'Content', which can be used in a
-- Yesod 'Response'.
hamletToContent :: Monad mo
                => HtmlUrl (Route master) -> GGHandler sub master mo Content
hamletToContent h = do
    render <- getUrlRenderParams
    return $ toContent $ h render

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
hamletToRepHtml :: Monad mo
                => HtmlUrl (Route master) -> GGHandler sub master mo RepHtml
hamletToRepHtml = liftM RepHtml . hamletToContent

-- | Get the request\'s 'W.Request' value.
waiRequest :: Monad mo => GGHandler sub master mo W.Request
waiRequest = reqWaiRequest `liftM` getRequest

getMessageRender :: (Monad mo, RenderMessage master message) => GGHandler s master mo (message -> Text)
getMessageRender = do
    m <- getYesod
    l <- reqLangs `liftM` getRequest
    return $ renderMessage m l
