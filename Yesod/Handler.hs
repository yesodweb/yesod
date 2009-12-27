{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-} -- FIXME remove
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

import Control.Exception hiding (Handler)
import Control.Applicative

import "transformers" Control.Monad.Trans
import Control.Monad.Attempt
import Control.Monad (liftM, ap)

import System.IO
import Data.Object.Html

--import Data.Typeable

------ Handler monad
newtype Handler yesod a = Handler {
    unHandler :: (RawRequest, yesod) -> IO ([Header], HandlerContents a)
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
    askRawRequest = Handler $ \(rr, _) -> return ([], HCContent rr)
    invalidParam _pt pn pe = invalidArgs [(pn, pe)]
    authRequired = permissionDenied

getYesod :: Handler yesod yesod
getYesod = Handler $ \(_, yesod) -> return ([], HCContent yesod)

runHandler :: Handler yesod RepChooser
           -> (ErrorResult -> Handler yesod RepChooser)
           -> RawRequest
           -> yesod
           -> [ContentType]
           -> IO Response
runHandler (Handler handler) eh rr y cts = do
    (headers, contents) <- Control.Exception.catch
        (handler (rr, y))
        (\e -> return ([], HCError (e :: Control.Exception.SomeException)))
    let contents' =
            case contents of
                HCError e -> Left $ InternalError $ show e
                HCSpecial e -> Left e
                HCContent a -> Right a
    case contents' of
        Left e -> do
            -- FIXME doesn't look right
            Response _ hs ct c <- runHandler (eh e) specialEh rr y cts
            let hs' = headers ++ hs ++ getHeaders e
            return $ Response (getStatus e) hs' ct c
        Right a -> do
            (ct, c) <- a cts
            return $ Response 200 headers ct c

specialEh :: ErrorResult -> Handler yesod RepChooser
specialEh er = do
    liftIO $ hPutStrLn stderr $ "Error handler errored out: " ++ show er
    return $ chooseRep $ toHtmlObject "Internal server error"
{- FIXME
class ToHandler a where
    toHandler :: a -> Handler

instance (Request r, ToHandler h) => ToHandler (r -> h) where
    toHandler f = parseRequest >>= toHandler . f

instance ToHandler Handler where
    toHandler = id

instance HasReps r HandlerIO => ToHandler (HandlerIO r) where
    toHandler = fmap reps

runHandler :: Handler
           -> RawRequest
           -> [ContentType]
           -> IO (Either (ErrorResult, [Header]) Response)
runHandler h rr cts = do
    --let (ares, _FIXMEheaders) =
    let x :: IO (Attempt (ContentType, Content), [Header])
        x =
         runWriterT $ runAttemptT $ runReaderT (joinHandler cts h) rr
        y :: IO (Attempt (Attempt (ContentType, Content), [Header]))
        y = takeAllExceptions x
    z <- y
    let z' :: Attempt (Attempt (ContentType, Content), [Header])
        z' = z
        a :: (Attempt (ContentType, Content), [Header])
        a = attempt (\e -> (failure e, [])) id z'
        (b, headers) = a
    return $ attempt (\e -> (Left (toErrorResult e, headers))) (Right . toResponse headers) b
    where
        takeAllExceptions :: MonadFailure SomeException m => IO x -> IO (m x)
        takeAllExceptions ioa =
            Control.Exception.catch (return `fmap` ioa) (\e -> return $ failure (e :: SomeException))
        toErrorResult :: Exception e => e -> ErrorResult
        toErrorResult e =
            case cast e of
                Just x -> x
                Nothing -> InternalError $ show e
        toResponse :: [Header] -> (ContentType, Content) -> Response
        toResponse hs (ct, c) = Response 200 hs ct c

joinHandler :: Monad m
            => [ContentType]
            -> m [RepT m]
            -> m (ContentType, Content)
joinHandler cts rs = do
    rs' <- rs
    let (ct, c) = chooseRep cts rs'
    c' <- c
    return (ct, c')
-}

{-
runHandler :: (ErrorResult -> Reps)
            -> (ContentType -> B.ByteString -> IO B.ByteString)
            -> [ContentType]
            -> Handler
            -> RawRequest
            -> IO Hack.Response
runHandler eh wrapper ctypesAll (HandlerT inside) rr = do
    let extraHeaders =
            case x of
                Left r -> getHeaders r
                Right _ -> []
    headers <- mapM toPair (headers' ++ extraHeaders)
    let outReps = either (reps . eh) reps x
    let statusCode =
            case x of
                Left r -> getStatus r
                Right _ -> 200
    (ctype, selectedRep) <- chooseRep outReps ctypesAll
    let languages = [] -- FIXME
    finalRep <- wrapper ctype $ selectedRep languages
    let headers'' = ("Content-Type", ctype) : headers
    return $! Hack.Response statusCode headers'' finalRep
-}

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
