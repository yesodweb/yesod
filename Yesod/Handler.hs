{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-} -- FIXME remove
{-# LANGUAGE FlexibleContexts #-}
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
      HandlerT
    , HandlerT' -- FIXME
    , HandlerIO
    , Handler
    , runHandler
    , liftIO
    , ToHandler (..)
      -- * Special handlers
    , redirect
    , notFound
      -- * Setting headers
    , addCookie
    , deleteCookie
    , header
    ) where

import Yesod.Request
import Yesod.Response

import Control.Exception hiding (Handler)

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Attempt

import Data.Typeable

------ Handler monad
type HandlerT m =
    ReaderT RawRequest (
        AttemptT (
            WriterT [Header] m
        )
    )
type HandlerIO = HandlerT IO
type Handler = HandlerIO [RepT HandlerIO]
type HandlerT' m a =
    ReaderT RawRequest (
        AttemptT (
            WriterT [Header] m
        )
    ) a

-- FIXME shouldn't call error here...
instance MonadRequestReader HandlerIO where
    askRawRequest = ask
    invalidParam _pt _pn _pe = error "invalidParam"
    authRequired = error "authRequired"
instance Exception e => Failure e HandlerIO where
    failure = error "HandlerIO failure"

class ToHandler a where
    toHandler :: a -> Handler

{- FIXME
instance (Request r, ToHandler h) => ToHandler (r -> h) where
    toHandler f = parseRequest >>= toHandler . f
-}

instance ToHandler Handler where
    toHandler = id

{- FIXME
instance HasReps r HandlerIO => ToHandler (HandlerIO r) where
    toHandler = fmap reps
-}

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
errorResult :: ErrorResult -> HandlerIO a
errorResult = lift . failure -- FIXME more instances in Attempt?

-- | Redirect to the given URL.
redirect :: String -> HandlerIO a
redirect = errorResult . Redirect

-- | Return a 404 not found page. Also denotes no handler available.
notFound :: HandlerIO a
notFound = errorResult NotFound

------- Headers
-- | Set the cookie on the client.
addCookie :: Monad m
          => Int -- ^ minutes to timeout
          -> String -- ^ key
          -> String -- ^ value
          -> HandlerT m ()
addCookie a b = addHeader . AddCookie a b

-- | Unset the cookie on the client.
deleteCookie :: Monad m => String -> HandlerT m ()
deleteCookie = addHeader . DeleteCookie

-- | Set an arbitrary header on the client.
header :: Monad m => String -> String -> HandlerT m ()
header a = addHeader . Header a

addHeader :: Monad m => Header -> HandlerT m ()
addHeader = lift . lift . tell . return
