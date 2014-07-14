{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.WebSockets
    ( -- * Core API
      WebSocketsT
    , webSockets
    , receiveData
    , receiveDataX
    , sendPing
    , sendPingX
    , sendClose
    , sendCloseX
    , sendTextData
    , sendTextDataX
    , sendBinaryData
    , sendBinaryDataX
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
import qualified Control.Exception              as E

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
#if MIN_VERSION_wai(3, 0, 0)
        Y.sendRawResponseNoConduit
#else
        Y.sendRawResponse
#endif
          $ \src sink -> control $ \runInIO -> WaiWS.runWebSockets
            WS.defaultConnectionOptions
            (WaiWS.getRequestHead req)
            (\pconn -> do
                conn <- WS.acceptRequest pconn
                runInIO $ runReaderT inner conn)
            src
            sink

-- | Receive a piece of data from the client.
--
-- Since 0.1.0
receiveData :: (MonadIO m, WS.WebSocketsData a) => WebSocketsT m a
receiveData = ReaderT $ liftIO . WS.receiveData

-- | Receive a piece of data from the client.
-- Execute IO () action on WebSocket Exception
-- Since 0.1.1.3
receiveDataX :: (MonadIO m, WS.WebSocketsData a) => IO () -> a -> WebSocketsT m a
receiveDataX ex d = ReaderT $ \c -> liftIO $ (WS.receiveData c) `E.catch` (\(_ :: E.SomeException) -> ex >> return d)

-- | Send a textual message to the client.
--
-- Since 0.1.0
sendTextData :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendTextData x = ReaderT $ liftIO . flip WS.sendTextData x

-- | Send a textual message to the client.
-- Execute IO () action on WebSocket Exception
-- Since 0.1.1.3
sendTextDataX :: (MonadIO m, WS.WebSocketsData a) => IO () -> a -> WebSocketsT m ()
sendTextDataX ex x = ReaderT $ \c -> liftIO $ (flip WS.sendTextData x $ c) `E.catch` (\(_ :: E.SomeException) -> ex)

-- | Send a binary message to the client.
--
-- Since 0.1.0
sendBinaryData :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendBinaryData x = ReaderT $ liftIO . flip WS.sendBinaryData x

-- | Send a binary message to the client.
-- Execute IO () action on WebSocket Exception
-- Since 0.1.1.3
sendBinaryDataX :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendBinaryDataX ex x = ReaderT $ \c -> liftIO $ (flip WS.sendBinaryData x $ c) `E.catch` (\(_ :: E.SomeException) -> ex)


-- | Send a ping message to the client.
--
-- Since 0.1.1.3
sendPing :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendPing x = ReaderT $ liftIO $ flip WS.sendPing x

-- | Send a ping message to the client. 
-- Execute IO () action on WebSocket Exception
-- Since 0.1.1.3
sendPingX :: (MonadIO m, WS.WebSocketsData a) => IO () -> a -> WebSocketsT m ()
sendPingX ex x = ReaderT $ \c -> liftIO $ (flip WS.sendPing x $ c) `E.catch` (\(_ :: E.SomeException) -> ex)

-- | Send a close request to the client. 
-- 
-- Since 0.1.1.3
sendClose :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendClose x = ReaderT $ liftIO . flip WS.sendClose x

-- | Send a close request to the client. 
-- Execute IO () action on WebSocket Exception
-- Since 0.1.1.3
sendCloseX :: (MonadIO m, WS.WebSocketsData a) => IO () -> a -> WebSocketsT m ()
sendCloseX ex x = ReaderT $ \c -> liftIO $ (flip WS.sendClose x $ c) `E.catch` (\(_ :: E.SomeException) -> ex)

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
