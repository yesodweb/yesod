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
      -- ** Errors
    , notFound
    , badMethod
    , permissionDenied
    , invalidArgs
      -- ** Short-circuit responses.
    , sendFile
    , sendResponse
      -- ** Calling foreign subsite handlers
    , runSubHandler
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
      -- * Internal Yesod
    , runHandler
    , YesodApp (..)
    , toMasterHandler
    , localNoCurrent
    , HandlerData
    , ErrorResponse (..)
#if TEST
    , testSuite
#endif
    ) where

import Prelude hiding (catch)
import Yesod.Request
import Yesod.Internal
import Data.Neither
import Data.Time (UTCTime)

import Control.Exception hiding (Handler, catch, finally)
import qualified Control.Exception as E
import Control.Applicative

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import System.IO
import qualified Network.Wai as W
import Control.Failure (Failure (failure))

import Text.Hamlet

import Control.Monad.Invert (MonadInvertIO (..))
import Control.Monad (liftM)
import qualified Data.Map as Map

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)
import Yesod.Content hiding (testSuite)
import Data.IORef
#else
import Yesod.Content
#endif

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
handlerSubData tm ts route hd = hd
    { handlerSub = ts $ handlerMaster hd
    , handlerToMaster = tm
    , handlerRoute = Just route
    }

-- | Used internally for promoting subsite handler functions to master site
-- handler functions. Should not be needed by users.
toMasterHandler :: (Route sub -> Route master)
                -> (master -> sub)
                -> Route sub
                -> GHandler sub master a
                -> GHandler master master a
toMasterHandler tm ts route (GHandler h) =
    GHandler $ withReaderT (handlerSubData tm ts route) h

-- | A generic handler monad, which can have a different subsite and master
-- site. This monad is a combination of 'ReaderT' for basic arguments, a
-- 'WriterT' for headers and session, and an 'MEitherT' monad for handling
-- special responses. It is declared as a newtype to make compiler errors more
-- readable.
newtype GHandler sub master a =
    GHandler
        { unGHandler :: GHInner sub master a
        }
    deriving (Functor, Applicative, Monad, MonadIO)

type GHInner s m =
    ReaderT (HandlerData s m) (
    MEitherT HandlerContents (
    WriterT (Endo [Header]) (
    StateT SessionMap ( -- session
    IO
    ))))

type SessionMap = Map.Map String String

instance MonadInvertIO (GHandler s m) where
    newtype InvertedIO (GHandler s m) a =
        InvGHandlerIO
            { runInvGHandlerIO :: InvertedIO (GHInner s m) a
            }
    type InvertedArg (GHandler s m) = (HandlerData s m, (SessionMap, ()))
    invertIO = liftM (fmap InvGHandlerIO) . invertIO . unGHandler
    revertIO f = GHandler $ revertIO $ liftM runInvGHandlerIO . f

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
    -> IO (W.Status, [Header], ContentType, Content, SessionMap)
    }

data HandlerContents =
      HCContent ChooseRep
    | HCError ErrorResponse
    | HCSendFile ContentType FilePath
    | HCRedirect RedirectType String

instance Failure ErrorResponse (GHandler sub master) where
    failure = GHandler . lift . throwMEither . HCError
instance RequestReader (GHandler sub master) where
    getRequest = handlerRequest <$> GHandler ask

-- | Get the sub application argument.
getYesodSub :: GHandler sub master sub
getYesodSub = handlerSub <$> GHandler ask

-- | Set the subsite in HandlerData
setHandlerSub :: YesodSubRoute sub' master => sub' -> HandlerData sub master -> HandlerData sub' master
setHandlerSub s (HandlerData r _ m _ rn _) = HandlerData r s m Nothing rn $ fromSubRoute s m

-- | Run a handler from another subsite
runSubHandler :: YesodSubRoute sub' master => sub' -> GHandler sub' master a -> GHandler sub master a
runSubHandler sub handler = do 
  hd <- setHandlerSub sub <$> GHandler ask
  session <- getSession
  GHandler $ do
    let toErrorHandler =
            InternalError
            . (show :: Control.Exception.SomeException -> String)
    ((contents, headers), finalSession) <- liftIO $ flip runStateT session
                       $ runWriterT
                       $ runMEitherT
                       $ flip runReaderT hd
                       $ unGHandler handler
    lift $ lift $ lift $ put finalSession
    lift $ MEitherT $ return contents


-- | Get the master site appliation argument.
getYesod :: GHandler sub master master
getYesod = handlerMaster <$> GHandler ask

-- | Get the URL rendering function.
getUrlRender :: GHandler sub master (Route master -> String)
getUrlRender = do
    x <- handlerRender <$> GHandler ask
    return $ flip x []

-- | The URL rendering function with query-string parameters.
getUrlRenderParams :: GHandler sub master (Route master -> [(String, String)] -> String)
getUrlRenderParams = handlerRender <$> GHandler ask

-- | Get the route requested by the user. If this is a 404 response- where the
-- user requested an invalid route- this function will return 'Nothing'.
getCurrentRoute :: GHandler sub master (Maybe (Route sub))
getCurrentRoute = handlerRoute <$> GHandler ask

-- | Get the function to promote a route for a subsite to a route for the
-- master site.
getRouteToMaster :: GHandler sub master (Route sub -> Route master)
getRouteToMaster = handlerToMaster <$> GHandler ask

-- | Function used internally by Yesod in the process of converting a
-- 'GHandler' into an 'W.Application'. Should not be needed by users.
runHandler :: HasReps c
           => GHandler sub master c
           -> (Route master -> [(String, String)] -> String)
           -> Maybe (Route sub)
           -> (Route sub -> Route master)
           -> master
           -> (master -> sub)
           -> YesodApp
runHandler handler mrender sroute tomr ma tosa =
  YesodApp $ \eh rr cts initSession -> do
    let toErrorHandler =
            InternalError
          . (show :: Control.Exception.SomeException -> String)
    let hd = HandlerData
            { handlerRequest = rr
            , handlerSub = tosa ma
            , handlerMaster = ma
            , handlerRoute = sroute
            , handlerRender = mrender
            , handlerToMaster = tomr
            }
    ((contents', headers), finalSession) <- E.catch (
        flip runStateT initSession
      $ runWriterT
      $ runMEitherT
      $ flip runReaderT hd
      $ unGHandler handler
        ) (\e -> return ((MLeft $ HCError $ toErrorHandler e, id), initSession))
    let contents = meither id (HCContent . chooseRep) contents'
    let handleError e = do
            (_, hs, ct, c, sess) <- unYesodApp (eh e) safeEh rr cts finalSession
            let hs' = headers hs
            return (getStatus e, hs', ct, c, sess)
    let sendFile' ct fp =
            return (W.status200, headers [], ct, W.ResponseFile fp, finalSession)
    case contents of
        HCContent a -> do
            (ct, c) <- chooseRep a cts
            return (W.status200, headers [], ct, c, finalSession)
        HCError e -> handleError e
        HCRedirect rt loc -> do
            let hs = Header "Location" loc : headers []
            return (getRedirectStatus rt, hs, typePlain, emptyContent,
                    finalSession)
        HCSendFile ct fp -> E.catch
            (sendFile' ct fp)
            (handleError . toErrorHandler)

safeEh :: ErrorResponse -> YesodApp
safeEh er = YesodApp $ \_ _ _ session -> do
    liftIO $ hPutStrLn stderr $ "Error handler errored out: " ++ show er
    return (W.status500, [], typePlain, toContent "Internal Server Error",
            session)

-- | Redirect to the given route.
redirect :: RedirectType -> Route master -> GHandler sub master a
redirect rt url = redirectParams rt url []

-- | Redirects to the given route with the associated query-string parameters.
redirectParams :: RedirectType -> Route master -> [(String, String)]
               -> GHandler sub master a
redirectParams rt url params = do
    r <- getUrlRenderParams
    redirectString rt $ r url params

-- | Redirect to the given URL.
redirectString :: RedirectType -> String -> GHandler sub master a
redirectString rt = GHandler . lift . throwMEither . HCRedirect rt

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
    maybe (redirect rt def) (redirectString rt) mdest

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
sendFile ct = GHandler . lift . throwMEither . HCSendFile ct

-- | Bypass remaining handler code and output the given content.
sendResponse :: HasReps c => c -> GHandler sub master a
sendResponse = GHandler . lift . throwMEither . HCContent . chooseRep

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
          -> String -- ^ key
          -> String -- ^ value
          -> GHandler sub master ()
setCookie a b = addHeader . AddCookie a b

-- | Unset the cookie on the client.
deleteCookie :: String -> GHandler sub master ()
deleteCookie = addHeader . DeleteCookie

-- | Set the language in the user session. Will show up in 'languages' on the
-- next request.
setLanguage :: String -> GHandler sub master ()
setLanguage = setSession langKey

-- | Set an arbitrary response header.
setHeader :: String -> String -> GHandler sub master ()
setHeader a = addHeader . Header a

-- | Set the Cache-Control header to indicate this response should be cached
-- for the given number of seconds.
cacheSeconds :: Int -> GHandler s m ()
cacheSeconds i = setHeader "Cache-Control" $ concat
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
expiresAt = setHeader "Expires" . formatRFC1123

-- | Set a variable in the user's session.
--
-- The session is handled by the clientsession package: it sets an encrypted
-- and hashed cookie on the client. This ensures that all data is secure and
-- not tampered with.
setSession :: String -- ^ key
           -> String -- ^ value
           -> GHandler sub master ()
setSession k = GHandler . lift . lift . lift . modify . Map.insert k

-- | Unsets a session variable. See 'setSession'.
deleteSession :: String -> GHandler sub master ()
deleteSession = GHandler . lift . lift . lift . modify . Map.delete

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
    m <- lift $ lift $ lift get
    return $ Map.lookup n m

-- | Get all session variables.
getSession :: GHandler s m SessionMap
getSession = GHandler $ lift $ lift $ lift get

#if TEST

testSuite :: Test
testSuite = testGroup "Yesod.Handler"
    [
    ]

#endif
