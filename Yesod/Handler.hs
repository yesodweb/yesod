{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
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
    , getYesod
    , getYesodMaster
    , getUrlRender
    , getUrlRenderMaster
    , getRoute
    , getRouteMaster
    , runHandler
    , liftIO
    , YesodApp (..)
    , Routes
      -- * Special handlers
    , redirect
    , sendFile
    , notFound
    , badMethod
    , permissionDenied
    , invalidArgs
      -- * Setting headers
    , addCookie
    , deleteCookie
    , header
    ) where

import Yesod.Request
import Yesod.Response
import Web.Mime

import Control.Exception hiding (Handler)
import Control.Applicative

import "transformers" Control.Monad.IO.Class
import Control.Monad.Attempt
import Control.Monad (liftM, ap)

import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as W

import Data.Convertible.Text (cs)

type family Routes y

data HandlerData sub master = HandlerData
    { handlerRequest :: Request
    , handlerSub :: sub
    , handlerMaster :: master
    , handlerRoute :: Maybe (Routes sub)
    , handlerRender :: (Routes master -> String)
    , handlerToMaster :: Routes sub -> Routes master
    }

newtype YesodApp = YesodApp
    { unYesodApp
    :: (ErrorResponse -> YesodApp)
    -> Request
    -> [ContentType]
    -> IO Response
    }

------ Handler monad
newtype GHandler sub master a = Handler {
    unHandler :: HandlerData sub master
              -> IO ([Header], HandlerContents a)
}
type Handler yesod = GHandler yesod yesod

data HandlerContents a =
      HCSpecial SpecialResponse
    | HCError ErrorResponse
    | HCContent a

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
                (HCError e) -> return ([], HCError e)
                (HCSpecial e) -> return ([], HCSpecial e)
                (HCContent a) -> unHandler (f a) rr
        return (headers ++ headers', c')
instance MonadIO (GHandler sub master) where
    liftIO i = Handler $ \_ -> i >>= \i' -> return ([], HCContent i')
instance Failure ErrorResponse (GHandler sub master) where
    failure e = Handler $ \_ -> return ([], HCError e)
instance RequestReader (GHandler sub master) where
    getRequest = Handler $ \r -> return ([], HCContent $ handlerRequest r)

getData :: GHandler sub master (HandlerData sub master)
getData = Handler $ \r -> return ([], HCContent r)

getYesod :: GHandler sub master sub
getYesod = handlerSub <$> getData

getYesodMaster :: GHandler sub master master
getYesodMaster = handlerMaster <$> getData

getUrlRender :: GHandler sub master (Routes sub -> String)
getUrlRender = do
    d <- getData
    return $ handlerRender d . handlerToMaster d

getUrlRenderMaster :: GHandler sub master (Routes master -> String)
getUrlRenderMaster = handlerRender <$> getData

getRoute :: GHandler sub master (Maybe (Routes sub))
getRoute = handlerRoute <$> getData

getRouteMaster :: GHandler sub master (Maybe (Routes master))
getRouteMaster = do
    d <- getData
    return $ handlerToMaster d <$> handlerRoute d

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
        (unHandler handler $ HandlerData
            { handlerRequest = rr
            , handlerSub = tosa ma
            , handlerMaster = ma
            , handlerRoute = sroute
            , handlerRender = mrender
            , handlerToMaster = tomr
            })
        (\e -> return ([], HCError $ toErrorHandler e))
    let handleError e = do
            Response _ hs ct c <- unYesodApp (eh e) safeEh rr cts
            let hs' = headers ++ hs
            return $ Response (getStatus e) hs' ct c
    let sendFile' ct fp = do
            c <- BL.readFile fp
            return $ Response W.Status200 headers ct $ cs c
    case contents of
        HCError e -> handleError e
        HCSpecial (Redirect rt loc) -> do
            let hs = Header "Location" loc : headers
            return $ Response (getRedirectStatus rt) hs TypePlain $ cs ""
        HCSpecial (SendFile ct fp) -> Control.Exception.catch
            (sendFile' ct fp)
            (handleError . toErrorHandler)
        HCContent a -> do
            (ct, c) <- chooseRep a cts
            return $ Response W.Status200 headers ct c

safeEh :: ErrorResponse -> YesodApp
safeEh er = YesodApp $ \_ _ _ -> do
    liftIO $ hPutStrLn stderr $ "Error handler errored out: " ++ show er
    return $ Response W.Status500 [] TypePlain $ cs "Internal Server Error"

------ Special handlers
specialResponse :: SpecialResponse -> GHandler sub master a
specialResponse er = Handler $ \_ -> return ([], HCSpecial er)

-- | Redirect to the given URL.
redirect :: RedirectType -> String -> GHandler sub master a
redirect rt = specialResponse . Redirect rt

sendFile :: ContentType -> FilePath -> GHandler sub master a
sendFile ct = specialResponse . SendFile ct

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: Failure ErrorResponse m => m a
notFound = failure NotFound

badMethod :: (RequestReader m, Failure ErrorResponse m) => m a
badMethod = do
    w <- waiRequest
    failure $ BadMethod $ cs $ W.methodToBS $ W.requestMethod w

permissionDenied :: Failure ErrorResponse m => m a
permissionDenied = failure PermissionDenied

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
