{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
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
    , getYesod
    , runHandler
    , liftIO
      -- * Special handlers
    , redirect
    , sendFile
    , notFound
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
import Data.Object.Html
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as W

data HandlerData yesod = HandlerData Request yesod

------ Handler monad
newtype Handler yesod a = Handler {
    unHandler :: HandlerData yesod
              -> IO ([Header], HandlerContents a)
}
data HandlerContents a =
      HCSpecial SpecialResponse
    | HCError ErrorResponse
    | HCContent a

instance Functor (Handler yesod) where
    fmap = liftM
instance Applicative (Handler yesod) where
    pure = return
    (<*>) = ap
instance Monad (Handler yesod) where
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
instance MonadIO (Handler yesod) where
    liftIO i = Handler $ \_ -> i >>= \i' -> return ([], HCContent i')
instance Failure ErrorResponse (Handler yesod) where
    failure e = Handler $ \_ -> return ([], HCError e)
instance RequestReader (Handler yesod) where
    getRequest = Handler $ \(HandlerData rr _)
                        -> return ([], HCContent rr)

getYesod :: Handler yesod yesod
getYesod = Handler $ \(HandlerData _ yesod) -> return ([], HCContent yesod)

runHandler :: Handler yesod ChooseRep
           -> (ErrorResponse -> Handler yesod ChooseRep)
           -> Request
           -> yesod
           -> [ContentType]
           -> IO Response
runHandler handler eh rr y cts = do
    let toErrorHandler =
            InternalError
          . (show :: Control.Exception.SomeException -> String)
    (headers, contents) <- Control.Exception.catch
        (unHandler handler $ HandlerData rr y)
        (\e -> return ([], HCError $ toErrorHandler e))
    let handleError e = do
            Response _ hs ct c <- runHandler (eh e) safeEh rr y cts
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
            (ct, c) <- a cts
            return $ Response W.Status200 headers ct c

safeEh :: ErrorResponse -> Handler yesod ChooseRep
safeEh er = do
    liftIO $ hPutStrLn stderr $ "Error handler errored out: " ++ show er
    return $ chooseRep
        ( Tag "title" [] $ cs "Internal Server Error"
        , toHtmlObject "Internal server error"
        )

------ Special handlers
specialResponse :: SpecialResponse -> Handler yesod a
specialResponse er = Handler $ \_ -> return ([], HCSpecial er)

-- | Redirect to the given URL.
redirect :: RedirectType -> String -> Handler yesod a
redirect rt = specialResponse . Redirect rt

sendFile :: ContentType -> FilePath -> Handler yesod a
sendFile ct = specialResponse . SendFile ct

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: Failure ErrorResponse m => m a
notFound = failure NotFound

permissionDenied :: Failure ErrorResponse m => m a
permissionDenied = failure PermissionDenied

invalidArgs :: Failure ErrorResponse m => [(ParamName, String)] -> m a
invalidArgs = failure . InvalidArgs

------- Headers
-- | Set the cookie on the client.
addCookie :: Int -- ^ minutes to timeout
          -> String -- ^ key
          -> String -- ^ value
          -> Handler yesod ()
addCookie a b = addHeader . AddCookie a b

-- | Unset the cookie on the client.
deleteCookie :: String -> Handler yesod ()
deleteCookie = addHeader . DeleteCookie

-- | Set an arbitrary header on the client.
header :: String -> String -> Handler yesod ()
header a = addHeader . Header a

addHeader :: Header -> Handler yesod ()
addHeader h = Handler $ \_ -> return ([h], HCContent ())
