{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.WebSockets
    ( -- * Core API
      WebSocketsT
    , webSockets
    , receiveData
    , sendTextData
    , sendBinaryData
      -- * Conduit API
    , sourceWS
    , sinkWSText
    , sinkWSBinary
      -- * Async helpers
    , race
    , race_
    , concurrently
    , concurrently_
    ) where

import qualified Control.Concurrent.Async       as A
import           Control.Monad                  (forever, void, when)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Control    (control)
import           Control.Monad.Trans.Control    (MonadBaseControl (liftBaseWith, restoreM))
import           Control.Monad.Trans.Reader     (ReaderT (ReaderT, runReaderT))
import qualified Data.Conduit                   as C
import qualified Data.Conduit.List              as CL
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import qualified Yesod.Core                     as Y

-- | A transformer for a WebSockets handler.
--
-- Since 0.1.0
type WebSocketsT = ReaderT WS.Connection

-- | Attempt to run a WebSockets handler. This function first checks if the
-- client initiated a WebSockets connection and, if so, runs the provided
-- application, short-circuiting the rest of your handler. If the client did
-- not request a WebSockets connection, the rest of your handler will be called
-- instead.
--
-- Since 0.1.0
webSockets :: (Y.MonadBaseControl IO m, Y.MonadHandler m) => WebSocketsT m () -> m ()
webSockets inner = do
    req <- Y.waiRequest
    when (WaiWS.isWebSocketsReq req) $
        Y.sendRawResponseNoConduit
          $ \src sink -> control $ \runInIO -> WaiWS.runWebSockets
            WS.defaultConnectionOptions
            (WaiWS.getRequestHead req)
            (\pconn -> do
                conn <- WS.acceptRequest pconn
                WS.forkPingThread conn 30
                runInIO $ runReaderT inner conn)
            src
            sink

-- | Receive a piece of data from the client.
--
-- Since 0.1.0
receiveData :: (MonadIO m, WS.WebSocketsData a) => WebSocketsT m a
receiveData = ReaderT $ liftIO . WS.receiveData

-- | Send a textual message to the client.
--
-- Since 0.1.0
sendTextData :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendTextData x = ReaderT $ liftIO . flip WS.sendTextData x

-- | Send a binary message to the client.
--
-- Since 0.1.0
sendBinaryData :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendBinaryData x = ReaderT $ liftIO . flip WS.sendBinaryData x

-- | A @Source@ of WebSockets data from the user.
--
-- Since 0.1.0
sourceWS :: (MonadIO m, WS.WebSocketsData a) => C.Producer (WebSocketsT m) a
sourceWS = forever $ Y.lift receiveData >>= C.yield

-- | A @Sink@ for sending textual data to the user.
--
-- Since 0.1.0
sinkWSText :: (MonadIO m, WS.WebSocketsData a) => C.Consumer a (WebSocketsT m) ()
sinkWSText = CL.mapM_ sendTextData

-- | A @Sink@ for sending binary data to the user.
--
-- Since 0.1.0
sinkWSBinary :: (MonadIO m, WS.WebSocketsData a) => C.Consumer a (WebSocketsT m) ()
sinkWSBinary = CL.mapM_ sendBinaryData

-- | Generalized version of 'A.race'.
--
-- Since 0.1.0
race :: MonadBaseControl IO m => m a -> m b -> m (Either a b)
race x y = liftBaseWith (\run -> A.race (run x) (run y))
    >>= either (fmap Left . restoreM) (fmap Right . restoreM)

-- | Generalized version of 'A.race_'.
--
-- Since 0.1.0
race_ :: MonadBaseControl IO m => m a -> m b -> m ()
race_ x y = void $ race x y

-- | Generalized version of 'A.concurrently'. Note that if your underlying
-- monad has some kind of mutable state, the state from the second action will
-- overwrite the state from the first.
--
-- Since 0.1.0
concurrently :: MonadBaseControl IO m => m a -> m b -> m (a, b)
concurrently x y = do
    (resX, resY) <- liftBaseWith $ \run -> A.concurrently (run x) (run y)
    x' <- restoreM resX
    y' <- restoreM resY
    return (x', y')

-- | Run two actions concurrently (like 'A.concurrently'), but discard their
-- results and any modified monadic state.
--
-- Since 0.1.0
concurrently_ :: MonadBaseControl IO m => m a -> m b -> m ()
concurrently_ x y = void $ liftBaseWith $ \run -> A.concurrently (run x) (run y)
