{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
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
      -- * Special responses
      -- ** Redirecting
    , RedirectType (..)
    , redirect
    , redirectParams
    , redirectString
    , redirectToPost
      -- ** Errors
    , notFound
    , badMethod
    , permissionDenied
    , invalidArgs
      -- ** Short-circuit responses.
    , sendFile
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
    , setUltDest'
    , redirectUltDest
      -- ** Messages
    , setMessage
    , getMessage
      -- * Helpers for specific content
      -- ** Hamlet
    , hamletToContent
    , hamletToRepHtml
      -- ** Misc
    , newIdent
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
#if TEST
    , handlerTestSuite
#endif
    ) where

import Prelude hiding (catch)
import Yesod.Request
import Yesod.Internal
import Data.Time (UTCTime)

import Control.Exception hiding (Handler, catch, finally)
import qualified Control.Exception as E
import Control.Applicative

import Control.Monad (liftM)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Error (throwError, ErrorT (runErrorT), Error (..))

import System.IO
import qualified Network.Wai as W
import Control.Failure (Failure (failure))

import Text.Hamlet

import Control.Monad.IO.Peel (MonadPeelIO)
import qualified Data.Map as Map
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee (..))
import Network.Wai.Parse (parseHttpAccept)

#if TEST
import Test.Framework (testGroup, Test)
#endif

import Yesod.Content
import Data.Maybe (fromMaybe)
import Web.Cookie (SetCookie (..), renderSetCookie)
import Blaze.ByteString.Builder (toByteString)
import Data.Enumerator (run_, ($$))
import Control.Arrow (second, (***))
import qualified Network.Wai.Parse as NWP

-- | The type-safe URLs associated with a site argument.
type family Route a

class YesodSubRoute s y where
    fromSubRoute :: s -> y -> Route s -> Route y

data HandlerData sub master = HandlerData
    { handlerRequest :: Request
    , handlerSub :: sub
    , handlerMaster :: master
    , handlerRoute :: Maybe (Route sub)
    , handlerRender :: (Route master -> [(String, String)] -> String)
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
                -> GHandler sub master a
                -> GHandler sub' master a
toMasterHandler tm ts route (GHandler h) =
    GHandler $ withReaderT (handlerSubData tm ts route) h

toMasterHandlerDyn :: (Route sub -> Route master)
                   -> GHandler sub' master sub
                   -> Route sub
                   -> GHandler sub master a
                   -> GHandler sub' master a
toMasterHandlerDyn tm getSub route (GHandler h) = do
    sub <- getSub
    GHandler $ withReaderT (handlerSubData tm (const sub) route) h

class SubsiteGetter g m s | g -> s where
  runSubsiteGetter :: g -> m s

instance (master ~ master'
         ) => SubsiteGetter (master -> sub) (GHandler anySub master') sub where
  runSubsiteGetter getter = do
    y <- getYesod
    return $ getter y

instance (anySub ~ anySub'
         ,master ~ master'
         ) => SubsiteGetter (GHandler anySub master sub) (GHandler anySub' master') sub where
  runSubsiteGetter = id

toMasterHandlerMaybe :: (Route sub -> Route master)
                     -> (master -> sub)
                     -> Maybe (Route sub)
                     -> GHandler sub master a
                     -> GHandler sub' master a
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
    deriving (Functor, Applicative, Monad, MonadIO, MonadPeelIO)

instance MonadTrans (GGHandler s m) where
    lift = GHandler . lift . lift . lift . lift

type GHandler sub master = GGHandler sub master (Iteratee ByteString IO)

data GHState = GHState
    { ghsSession :: SessionMap
    , ghsRBC :: Maybe RequestBodyContents
    , ghsIdent :: Int
    }

type GHInner s m monad =
    ReaderT (HandlerData s m) (
    ErrorT HandlerContents (
    WriterT (Endo [Header]) (
    StateT GHState (
    monad
    ))))

type SessionMap = Map.Map String String

type Endo a = a -> a

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
    | YARPlain W.Status [Header] ContentType Content SessionMap

data HandlerContents =
      HCContent W.Status ChooseRep
    | HCError ErrorResponse
    | HCSendFile ContentType FilePath
    | HCRedirect RedirectType ByteString
    | HCCreated ByteString
    | HCWai W.Response

instance Error HandlerContents where
    strMsg = HCError . InternalError

instance Monad monad => Failure ErrorResponse (GGHandler sub master monad) where
    failure = GHandler . lift . throwError . HCError
instance RequestReader (GHandler sub master) where
    getRequest = handlerRequest <$> GHandler ask
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
    fix1 = bsToChars *** bsToChars
    fix2 (x, NWP.FileInfo a b c) =
        (bsToChars x, FileInfo (bsToChars a) (bsToChars b) c)

-- | Get the sub application argument.
getYesodSub :: Monad m => GGHandler sub master m sub
getYesodSub = handlerSub `liftM` GHandler ask

-- | Get the master site appliation argument.
getYesod :: Monad m => GGHandler sub master m master
getYesod = handlerMaster `liftM` GHandler ask

-- | Get the URL rendering function.
getUrlRender :: Monad m => GGHandler sub master m (Route master -> String)
getUrlRender = do
    x <- handlerRender `liftM` GHandler ask
    return $ flip x []

-- | The URL rendering function with query-string parameters.
getUrlRenderParams
    :: Monad m
    => GGHandler sub master m (Route master -> [(String, String)] -> String)
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
           -> (Route master -> [(String, String)] -> String)
           -> Maybe (Route sub)
           -> (Route sub -> Route master)
           -> master
           -> sub
           -> YesodApp
runHandler handler mrender sroute tomr ma sa =
  YesodApp $ \eh rr cts initSession -> do
    let toErrorHandler =
            InternalError
          . (show :: Control.Exception.SomeException -> String)
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
        ) (\e -> return ((Left $ HCError $ toErrorHandler e, id), initSession))
    let contents = either id (HCContent W.status200 . chooseRep) contents'
    let handleError e = do
            yar <- unYesodApp (eh e) safeEh rr cts finalSession
            case yar of
                YARPlain _ hs ct c sess ->
                    let hs' = headers hs
                     in return $ YARPlain (getStatus e) hs' ct c sess
                YARWai _ -> return yar
    let sendFile' ct fp =
            return $ YARPlain W.status200 (headers []) ct (ContentFile fp) finalSession
    case contents of
        HCContent status a -> do
            (ct, c) <- liftIO $ chooseRep a cts
            return $ YARPlain status (headers []) ct c finalSession
        HCError e -> handleError e
        HCRedirect rt loc -> do
            let hs = Header "Location" loc : headers []
            return $ YARPlain
                (getRedirectStatus rt) hs typePlain emptyContent
                finalSession
        HCSendFile ct fp -> catchIter
            (sendFile' ct fp)
            (handleError . toErrorHandler)
        HCCreated loc -> do
            let hs = Header "Location" loc : headers []
            return $ YARPlain
                (W.Status 201 (S8.pack "Created"))
                hs
                typePlain
                emptyContent
                finalSession
        HCWai r -> return $ YARWai r

catchIter :: Exception e
          => Iteratee ByteString IO a
          -> (e -> Iteratee ByteString IO a)
          -> Iteratee ByteString IO a
catchIter (Iteratee mstep) f = Iteratee $ mstep `E.catch` (runIteratee . f)

safeEh :: ErrorResponse -> YesodApp
safeEh er = YesodApp $ \_ _ _ session -> do
    liftIO $ hPutStrLn stderr $ "Error handler errored out: " ++ show er
    return $ YARPlain
        W.status500
        []
        typePlain
        (toContent ("Internal Server Error" :: S.ByteString))
        session

-- | Redirect to the given route.
redirect :: RedirectType -> Route master -> GHandler sub master a
redirect rt url = redirectParams rt url []

-- | Redirects to the given route with the associated query-string parameters.
redirectParams :: RedirectType -> Route master -> [(String, String)]
               -> GHandler sub master a
redirectParams rt url params = do
    r <- getUrlRenderParams
    redirectString rt $ S8.pack $ r url params

-- | Redirect to the given URL.
redirectString :: RedirectType -> ByteString -> GHandler sub master a
redirectString rt = GHandler . lift . throwError . HCRedirect rt

ultDestKey :: String
ultDestKey = "_ULT"

-- | Sets the ultimate destination variable to the given route.
--
-- An ultimate destination is stored in the user session and can be loaded
-- later by 'redirectUltDest'.
setUltDest :: Route master -> GHandler sub master ()
setUltDest dest = do
    render <- getUrlRender
    setUltDestString $ render dest

-- | Same as 'setUltDest', but use the given string.
setUltDestString :: String -> GHandler sub master ()
setUltDestString = setSession ultDestKey

-- | Same as 'setUltDest', but uses the current page.
--
-- If this is a 404 handler, there is no current page, and then this call does
-- nothing.
setUltDest' :: GHandler sub master ()
setUltDest' = do
    route <- getCurrentRoute
    case route of
        Nothing -> return ()
        Just r -> do
            tm <- getRouteToMaster
            gets' <- reqGetParams <$> getRequest
            render <- getUrlRenderParams
            setUltDestString $ render (tm r) gets'

-- | Redirect to the ultimate destination in the user's session. Clear the
-- value from the session.
--
-- The ultimate destination is set with 'setUltDest'.
redirectUltDest :: RedirectType
                -> Route master -- ^ default destination if nothing in session
                -> GHandler sub master ()
redirectUltDest rt def = do
    mdest <- lookupSession ultDestKey
    deleteSession ultDestKey
    maybe (redirect rt def) (redirectString rt . S8.pack) mdest

msgKey :: String
msgKey = "_MSG"

-- | Sets a message in the user's session.
--
-- See 'getMessage'.
setMessage :: Html -> GHandler sub master ()
setMessage = setSession msgKey . lbsToChars . renderHtml

-- | Gets the message in the user's session, if available, and then clears the
-- variable.
--
-- See 'setMessage'.
getMessage :: GHandler sub master (Maybe Html)
getMessage = do
    mmsg <- fmap (fmap preEscapedString) $ lookupSession msgKey
    deleteSession msgKey
    return mmsg

-- | Bypass remaining handler code and output the given file.
--
-- For some backends, this is more efficient than reading in the file to
-- memory, since they can optimize file sending via a system call to sendfile.
sendFile :: ContentType -> FilePath -> GHandler sub master a
sendFile ct = GHandler . lift . throwError . HCSendFile ct

-- | Bypass remaining handler code and output the given content with a 200
-- status code.
sendResponse :: HasReps c => c -> GHandler sub master a
sendResponse = GHandler . lift . throwError . HCContent W.status200
             . chooseRep

-- | Bypass remaining handler code and output the given content with the given
-- status code.
sendResponseStatus :: HasReps c => W.Status -> c -> GHandler s m a
sendResponseStatus s = GHandler . lift . throwError . HCContent s
                     . chooseRep

-- | Send a 201 "Created" response with the given route as the Location
-- response header.
sendResponseCreated :: Route m -> GHandler s m a
sendResponseCreated url = do
    r <- getUrlRender
    GHandler $ lift $ throwError $ HCCreated $ S8.pack $ r url

-- | Send a 'W.Response'. Please note: this function is rarely
-- necessary, and will /disregard/ any changes to response headers and session
-- that you have already specified. This function short-circuits. It should be
-- considered only for very specific needs. If you are not sure if you need it,
-- you don't.
sendWaiResponse :: W.Response -> GHandler s m b
sendWaiResponse = GHandler . lift . throwError . HCWai

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: Failure ErrorResponse m => m a
notFound = failure NotFound

-- | Return a 405 method not supported page.
badMethod :: (RequestReader m, Failure ErrorResponse m) => m a
badMethod = do
    w <- waiRequest
    failure $ BadMethod $ bsToChars $ W.requestMethod w

-- | Return a 403 permission denied page.
permissionDenied :: Failure ErrorResponse m => String -> m a
permissionDenied = failure . PermissionDenied

-- | Return a 400 invalid arguments page.
invalidArgs :: Failure ErrorResponse m => [String] -> m a
invalidArgs = failure . InvalidArgs

------- Headers
-- | Set the cookie on the client.
setCookie :: Int -- ^ minutes to timeout
          -> ByteString -- ^ key
          -> ByteString -- ^ value
          -> GHandler sub master ()
setCookie a b = addHeader . AddCookie a b

-- | Unset the cookie on the client.
deleteCookie :: ByteString -> GHandler sub master ()
deleteCookie = addHeader . DeleteCookie

-- | Set the language in the user session. Will show up in 'languages' on the
-- next request.
setLanguage :: String -> GHandler sub master ()
setLanguage = setSession langKey

-- | Set an arbitrary response header.
setHeader :: W.ResponseHeader -> ByteString -> GHandler sub master ()
setHeader a = addHeader . Header a

-- | Set the Cache-Control header to indicate this response should be cached
-- for the given number of seconds.
cacheSeconds :: Int -> GHandler s m ()
cacheSeconds i = setHeader "Cache-Control" $ S8.pack $ concat
    [ "max-age="
    , show i
    , ", public"
    ]

-- | Set the Expires header to some date in 2037. In other words, this content
-- is never (realistically) expired.
neverExpires :: GHandler s m ()
neverExpires = setHeader "Expires" "Thu, 31 Dec 2037 23:55:55 GMT"

-- | Set an Expires header in the past, meaning this content should not be
-- cached.
alreadyExpired :: GHandler s m ()
alreadyExpired = setHeader "Expires" "Thu, 01 Jan 1970 05:05:05 GMT"

-- | Set an Expires header to the given date.
expiresAt :: UTCTime -> GHandler s m ()
expiresAt = setHeader "Expires" . S8.pack . formatRFC1123

-- | Set a variable in the user's session.
--
-- The session is handled by the clientsession package: it sets an encrypted
-- and hashed cookie on the client. This ensures that all data is secure and
-- not tampered with.
setSession :: String -- ^ key
           -> String -- ^ value
           -> GHandler sub master ()
setSession k = GHandler . lift . lift . lift . modify . modSession . Map.insert k

-- | Unsets a session variable. See 'setSession'.
deleteSession :: String -> GHandler sub master ()
deleteSession = GHandler . lift . lift . lift . modify . modSession . Map.delete

modSession :: (SessionMap -> SessionMap) -> GHState -> GHState
modSession f x = x { ghsSession = f $ ghsSession x }

-- | Internal use only, not to be confused with 'setHeader'.
addHeader :: Header -> GHandler sub master ()
addHeader = GHandler . lift . lift . tell . (:)

getStatus :: ErrorResponse -> W.Status
getStatus NotFound = W.status404
getStatus (InternalError _) = W.status500
getStatus (InvalidArgs _) = W.status400
getStatus (PermissionDenied _) = W.status403
getStatus (BadMethod _) = W.status405

getRedirectStatus :: RedirectType -> W.Status
getRedirectStatus RedirectPermanent = W.status301
getRedirectStatus RedirectTemporary = W.status302
getRedirectStatus RedirectSeeOther = W.status303

-- | Different types of redirects.
data RedirectType = RedirectPermanent
                  | RedirectTemporary
                  | RedirectSeeOther
    deriving (Show, Eq)

localNoCurrent :: GHandler s m a -> GHandler s m a
localNoCurrent =
    GHandler . local (\hd -> hd { handlerRoute = Nothing }) . unGHandler

-- | Lookup for session data.
lookupSession :: ParamName -> GHandler s m (Maybe ParamValue)
lookupSession n = GHandler $ do
    m <- fmap ghsSession $ lift $ lift $ lift get
    return $ Map.lookup n m

-- | Get all session variables.
getSession :: GHandler s m SessionMap
getSession = fmap ghsSession $ GHandler $ lift $ lift $ lift get

#if TEST

handlerTestSuite :: Test
handlerTestSuite = testGroup "Yesod.Handler"
    [
    ]

#endif

handlerToYAR :: (HasReps a, HasReps b)
             => m -- ^ master site foundation
             -> s -- ^ sub site foundation
             -> (Route s -> Route m)
             -> (Route m -> [(String, String)] -> String) -- ^ url render
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
                   -> [(W.ResponseHeader, ByteString)]

yarToResponse :: HeaderRenderer -> YesodAppResult -> W.Response
yarToResponse _ (YARWai a) = a
yarToResponse renderHeaders (YARPlain s hs ct c sessionFinal) =
    case c of
        ContentBuilder b mlen ->
            let hs' = maybe finalHeaders finalHeaders' mlen
             in W.ResponseBuilder s hs' b
        ContentFile fp -> W.ResponseFile s finalHeaders fp
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
           . fromMaybe S.empty
           . lookup "Accept"
           . W.requestHeaders

-- | Convert Header to a key/value pair.
headerToPair :: (Int -> UTCTime) -- ^ minutes -> expiration time
             -> Header
             -> (W.ResponseHeader, ByteString)
headerToPair getExpires (AddCookie minutes key value) =
    ("Set-Cookie", toByteString $ renderSetCookie $ SetCookie
        { setCookieName = key
        , setCookieValue = value
        , setCookiePath = Just "/" -- FIXME make a config option, or use approot?
        , setCookieExpires = Just $ getExpires minutes
        , setCookieDomain = Nothing
        })
headerToPair _ (DeleteCookie key) =
    ( "Set-Cookie"
    , key `S.append` "=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT"
    )
headerToPair _ (Header key value) = (key, value)

-- | Get a unique identifier.
newIdent :: Monad mo => GGHandler sub master mo String
newIdent = GHandler $ lift $ lift $ lift $ do
    x <- get
    let i' = ghsIdent x + 1
    put x { ghsIdent = i' }
    return $ "h" ++ show i'

-- | Redirect to a POST resource.
--
-- This is not technically a redirect; instead, it returns an HTML page with a
-- POST form, and some Javascript to automatically submit the form. This can be
-- useful when you need to post a plain link somewhere that needs to cause
-- changes on the server.
redirectToPost :: Route master -> GHandler sub master a
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
hamletToContent :: Hamlet (Route master) -> GHandler sub master Content
hamletToContent h = do
    render <- getUrlRenderParams
    return $ toContent $ h render

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
hamletToRepHtml :: Hamlet (Route master) -> GHandler sub master RepHtml
hamletToRepHtml = fmap RepHtml . hamletToContent
