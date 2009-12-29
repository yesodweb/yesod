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
    --, ToHandler (..)
      -- * Special handlers
    , redirect
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
import Yesod.Rep
import Yesod.Template

import Control.Exception hiding (Handler)
import Control.Applicative

import "transformers" Control.Monad.Trans
import Control.Monad.Attempt
import Control.Monad (liftM, ap)

import System.IO
import Data.Object.Html

------ Handler monad
newtype Handler yesod a = Handler {
    unHandler :: (RawRequest, yesod, TemplateGroup)
              -> IO ([Header], HandlerContents a)
}
data HandlerContents a =
    forall e. Exception e => HCError e
    | HCSpecial ErrorResult
    | HCContent a

instance Functor (Handler yesod) where
    fmap = liftM
instance Applicative (Handler yesod) where
    pure = return
    (<*>) = ap
instance Monad (Handler yesod) where
    fail = failureString -- We want to catch all exceptions anyway
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
instance Exception e => Failure e (Handler yesod) where
    failure e = Handler $ \_ -> return ([], HCError e)
instance MonadRequestReader (Handler yesod) where
    askRawRequest = Handler $ \(rr, _, _) -> return ([], HCContent rr)
    invalidParam _pt pn pe = invalidArgs [(pn, pe)]
    authRequired = permissionDenied

getYesod :: Handler yesod yesod
getYesod = Handler $ \(_, yesod, _) -> return ([], HCContent yesod)

instance HasTemplateGroup (Handler yesod) where
    getTemplateGroup = Handler $ \(_, _, tg) -> return ([], HCContent tg)

runHandler :: Handler yesod RepChooser
           -> (ErrorResult -> Handler yesod RepChooser)
           -> RawRequest
           -> yesod
           -> TemplateGroup
           -> [ContentType]
           -> IO Response
runHandler (Handler handler) eh rr y tg cts = do
    (headers, contents) <- Control.Exception.catch
        (handler (rr, y, tg))
        (\e -> return ([], HCError (e :: Control.Exception.SomeException)))
    let contents' =
            case contents of
                HCError e -> Left $ InternalError $ show e
                HCSpecial e -> Left e
                HCContent a -> Right a
    case contents' of
        Left e -> do
            Response _ hs ct c <- runHandler (eh e) specialEh rr y tg cts
            let hs' = headers ++ hs ++ getHeaders e
            return $ Response (getStatus e) hs' ct c
        Right a -> do
            (ct, c) <- a cts
            return $ Response 200 headers ct c

specialEh :: ErrorResult -> Handler yesod RepChooser
specialEh er = do
    liftIO $ hPutStrLn stderr $ "Error handler errored out: " ++ show er
    return $ chooseRep $ toHtmlObject "Internal server error"

------ Special handlers
errorResult :: ErrorResult -> Handler yesod a
errorResult er = Handler $ \_ -> return ([], HCSpecial er)

-- | Redirect to the given URL.
redirect :: String -> Handler yesod a
redirect = errorResult . Redirect

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: Handler yesod a
notFound = errorResult NotFound

permissionDenied :: Handler yesod a
permissionDenied = errorResult PermissionDenied

invalidArgs :: [(ParamName, ParamValue)] -> Handler yesod a
invalidArgs = errorResult . InvalidArgs

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
