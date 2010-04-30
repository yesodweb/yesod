{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
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
    ( -- * Handler monad
      Handler
    , GHandler
      -- ** Read information from handler
    , getYesod
    , getYesodMaster
    , getUrlRender
    , getUrlRenderMaster
    , getRoute
    , getRouteToMaster
      -- * Special responses
    , RedirectType (..)
    , redirect
    , redirectString
    , sendFile
    , notFound
    , badMethod
    , permissionDenied
    , invalidArgs
      -- * Setting headers
    , addCookie
    , deleteCookie
    , header
      -- * Internal Yesod
    , runHandler
    , YesodApp (..)
    ) where

import Yesod.Request
import Yesod.Content
import Yesod.Internal
import Yesod.Definitions
import Web.Mime

import Control.Exception hiding (Handler)
import Control.Applicative

#if TRANSFORMERS_02
import "transformers" Control.Monad.IO.Class
#else
import "transformers" Control.Monad.Trans
#endif
import Control.Monad.Attempt
import Control.Monad (liftM, ap)

import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as W

import Data.Convertible.Text (cs)

data HandlerData sub master = HandlerData
    { handlerRequest :: Request
    , handlerSub :: sub
    , handlerMaster :: master
    , handlerRoute :: Maybe (Routes sub)
    , handlerRender :: (Routes master -> String)
    , handlerToMaster :: Routes sub -> Routes master
    }

-- | A generic handler monad, which can have a different subsite and master
-- site. This monad is a combination of reader for basic arguments, a writer
-- for headers, and an error-type monad for handling special responses.
newtype GHandler sub master a = Handler {
    unHandler :: HandlerData sub master -> IO ([Header], HandlerContents a)
}

-- | A 'GHandler' limited to the case where the master and sub sites are the
-- same. This is the usual case for application writing; only code written
-- specifically as a subsite need been concerned with the more general variety.
type Handler yesod = GHandler yesod yesod

-- | An extension of the basic WAI 'W.Application' datatype to provide extra
-- features needed by Yesod. Users should never need to use this directly, as
-- the 'GHandler' monad and template haskell code should hide it away.
newtype YesodApp = YesodApp
    { unYesodApp
    :: (ErrorResponse -> YesodApp)
    -> Request
    -> [ContentType]
    -> IO (W.Status, [Header], ContentType, Content)
    }

data HandlerContents a =
      HCContent a
    | HCError ErrorResponse
    | HCSendFile ContentType FilePath
    | HCRedirect RedirectType String

instance Functor (GHandler sub master) where
    fmap = liftM
instance Applicative (GHandler sub master) where
    pure = return
    (<*>) = ap
instance Monad (GHandler sub master) where
    fail = failure . InternalError -- We want to catch all exceptions anyway
    return x = Handler $ \_ -> return ([], HCContent x)
    (Handler handler) >>= f = Handler $ \rr -> do
        (headers, c) <- handler rr
        (headers', c') <-
            case c of
                HCContent a -> unHandler (f a) rr
                HCError e -> return ([], HCError e)
                HCSendFile ct fp -> return ([], HCSendFile ct fp)
                HCRedirect rt url -> return ([], HCRedirect rt url)
        return (headers ++ headers', c')
instance MonadIO (GHandler sub master) where
    liftIO i = Handler $ \_ -> i >>= \i' -> return ([], HCContent i')
instance Failure ErrorResponse (GHandler sub master) where
    failure e = Handler $ \_ -> return ([], HCError e)
instance RequestReader (GHandler sub master) where
    getRequest = Handler $ \r -> return ([], HCContent $ handlerRequest r)

getData :: GHandler sub master (HandlerData sub master)
getData = Handler $ \r -> return ([], HCContent r)

-- | Get the application argument.
getYesod :: GHandler sub master sub
getYesod = handlerSub <$> getData

-- | Get the master site appliation argument.
getYesodMaster :: GHandler sub master master
getYesodMaster = handlerMaster <$> getData

-- | Get the URL rendering function.
getUrlRender :: GHandler sub master (Routes sub -> String)
getUrlRender = do
    d <- getData
    return $ handlerRender d . handlerToMaster d

-- | Get the URL rendering function for the master site.
getUrlRenderMaster :: GHandler sub master (Routes master -> String)
getUrlRenderMaster = handlerRender <$> getData

-- | Get the route requested by the user. If this is a 404 response- where the
-- user requested an invalid route- this function will return 'Nothing'.
getRoute :: GHandler sub master (Maybe (Routes sub))
getRoute = handlerRoute <$> getData

-- | Get the function to promote a route for a subsite to a route for the
-- master site.
getRouteToMaster :: GHandler sub master (Routes sub -> Routes master)
getRouteToMaster = handlerToMaster <$> getData

-- | Function used internally by Yesod in the process of converting a
-- 'GHandler' into an 'W.Application'. Should not be needed by users.
runHandler :: HasReps c
           => GHandler sub master c
           -> (Routes master -> String)
           -> Maybe (Routes sub)
           -> (Routes sub -> Routes master)
           -> master
           -> (master -> sub)
           -> YesodApp
runHandler handler mrender sroute tomr ma tosa = YesodApp $ \eh rr cts -> do
    let toErrorHandler =
            InternalError
          . (show :: Control.Exception.SomeException -> String)
    (headers, contents) <- Control.Exception.catch
        (unHandler handler HandlerData
            { handlerRequest = rr
            , handlerSub = tosa ma
            , handlerMaster = ma
            , handlerRoute = sroute
            , handlerRender = mrender
            , handlerToMaster = tomr
            })
        (\e -> return ([], HCError $ toErrorHandler e))
    let handleError e = do
            (_, hs, ct, c) <- unYesodApp (eh e) safeEh rr cts
            let hs' = headers ++ hs
            return (getStatus e, hs', ct, c)
    let sendFile' ct fp = do
            c <- BL.readFile fp
            return (W.Status200, headers, ct, cs c)
    case contents of
        HCContent a -> do
            (ct, c) <- chooseRep a cts
            return (W.Status200, headers, ct, c)
        HCError e -> handleError e
        HCRedirect rt loc -> do
            let hs = Header "Location" loc : headers
            return (getRedirectStatus rt, hs, TypePlain, cs "")
        HCSendFile ct fp -> Control.Exception.catch
            (sendFile' ct fp)
            (handleError . toErrorHandler)

safeEh :: ErrorResponse -> YesodApp
safeEh er = YesodApp $ \_ _ _ -> do
    liftIO $ hPutStrLn stderr $ "Error handler errored out: " ++ show er
    return (W.Status500, [], TypePlain, cs "Internal Server Error")

-- | Redirect to the given route.
redirect :: RedirectType -> Routes master -> GHandler sub master a
redirect rt url = do
    r <- getUrlRenderMaster
    redirectString rt $ r url

-- | Redirect to the given URL.
redirectString :: RedirectType -> String -> GHandler sub master a
redirectString rt url = Handler $ \_ -> return ([], HCRedirect rt url)

-- | Bypass remaining handler code and output the given file.
--
-- For some backends, this is more efficient than reading in the file to
-- memory, since they can optimize file sending via a system call to sendfile.
sendFile :: ContentType -> FilePath -> GHandler sub master a
sendFile ct fp = Handler $ \_ -> return ([], HCSendFile ct fp)

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: Failure ErrorResponse m => m a
notFound = failure NotFound

-- | Return a 405 method not supported page.
badMethod :: (RequestReader m, Failure ErrorResponse m) => m a
badMethod = do
    w <- waiRequest
    failure $ BadMethod $ cs $ W.methodToBS $ W.requestMethod w

-- | Return a 403 permission denied page.
permissionDenied :: Failure ErrorResponse m => m a
permissionDenied = failure PermissionDenied

-- | Return a 400 invalid arguments page.
invalidArgs :: Failure ErrorResponse m => [(ParamName, String)] -> m a
invalidArgs = failure . InvalidArgs

------- Headers
-- | Set the cookie on the client.
addCookie :: Int -- ^ minutes to timeout
          -> String -- ^ key
          -> String -- ^ value
          -> GHandler sub master ()
addCookie a b = addHeader . AddCookie a b

-- | Unset the cookie on the client.
deleteCookie :: String -> GHandler sub master ()
deleteCookie = addHeader . DeleteCookie

-- | Set an arbitrary header on the client.
header :: String -> String -> GHandler sub master ()
header a = addHeader . Header a

addHeader :: Header -> GHandler sub master ()
addHeader h = Handler $ \_ -> return ([h], HCContent ())

getStatus :: ErrorResponse -> W.Status
getStatus NotFound = W.Status404
getStatus (InternalError _) = W.Status500
getStatus (InvalidArgs _) = W.Status400
getStatus PermissionDenied = W.Status403
getStatus (BadMethod _) = W.Status405

getRedirectStatus :: RedirectType -> W.Status
getRedirectStatus RedirectPermanent = W.Status301
getRedirectStatus RedirectTemporary = W.Status302
getRedirectStatus RedirectSeeOther = W.Status303

-- | Different types of redirects.
data RedirectType = RedirectPermanent
                  | RedirectTemporary
                  | RedirectSeeOther
    deriving (Show, Eq)
