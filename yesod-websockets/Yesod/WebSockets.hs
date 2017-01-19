{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.WebSockets
    ( -- * Core API
      WebSocketsT
    , webSockets
    , webSocketsWith
    , webSocketsOptions
    , webSocketsOptionsWith
    , receiveData
    , receiveDataE
    , receiveDataMessageE
    , sendPing
    , sendPingE
    , sendClose
    , sendCloseE
    , sendTextData
    , sendTextDataE
    , sendBinaryData
    , sendBinaryDataE
    , sendDataMessageE
      -- * Conduit API
    , sourceWS
    , sinkWSText
    , sinkWSBinary
      -- * Async helpers
    , race
    , race_
    , concurrently
    , concurrently_
      -- * Re-exports from websockets
    , WS.defaultConnectionOptions
    , WS.ConnectionOptions (..)
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
import           Control.Exception (SomeException)
import           Control.Exception.Enclosed (tryAny)

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
webSockets = webSocketsOptions WS.defaultConnectionOptions

-- | Varient of 'webSockets' which allows you to specify
-- the WS.ConnectionOptions setttings when upgrading to a websocket connection.
--
-- Since 0.2.5
webSocketsOptions :: (Y.MonadBaseControl IO m, Y.MonadHandler m)
               => WS.ConnectionOptions
               -> WebSocketsT m ()
               -> m ()
#if MIN_VERSION_websockets(0,10,0)
webSocketsOptions opts = webSocketsOptionsWith opts $ const $ return $ Just $ WS.AcceptRequest Nothing []
#else
webSocketsOptions opts = webSocketsOptionsWith opts $ const $ return $ Just $ WS.AcceptRequest Nothing
#endif

-- | Varient of 'webSockets' which allows you to specify the 'WS.AcceptRequest'
-- setttings when upgrading to a websocket connection.
--
-- Since 0.2.4
webSocketsWith :: (Y.MonadBaseControl IO m, Y.MonadHandler m)
               => (WS.RequestHead -> m (Maybe WS.AcceptRequest))
               -- ^ A Nothing indicates that the websocket upgrade request should not happen
               -- and instead the rest of the handler will be called instead.  This allows
               -- you to use 'WS.getRequestSubprotocols' and only accept the request if
               -- a compatible subprotocol is given.  Also, the action runs before upgrading
               -- the request to websockets, so you can also use short-circuiting handler
               -- actions such as 'Y.invalidArgs'.
               -> WebSocketsT m ()
               -> m ()
webSocketsWith = webSocketsOptionsWith WS.defaultConnectionOptions

-- | Varient of 'webSockets' which allows you to specify both
-- the WS.ConnectionOptions and the 'WS.AcceptRequest'
-- setttings when upgrading to a websocket connection.
--
-- Since 0.2.5
webSocketsOptionsWith :: (Y.MonadBaseControl IO m, Y.MonadHandler m)
               => WS.ConnectionOptions
               -- ^ Custom websockets options
               -> (WS.RequestHead -> m (Maybe WS.AcceptRequest))
               -- ^ A Nothing indicates that the websocket upgrade request should not happen
               -- and instead the rest of the handler will be called instead.  This allows
               -- you to use 'WS.getRequestSubprotocols' and only accept the request if
               -- a compatible subprotocol is given.  Also, the action runs before upgrading
               -- the request to websockets, so you can also use short-circuiting handler
               -- actions such as 'Y.invalidArgs'.
               -> WebSocketsT m ()
               -> m ()
webSocketsOptionsWith wsConnOpts buildAr inner = do
    req <- Y.waiRequest
    when (WaiWS.isWebSocketsReq req) $ do
        let rhead = WaiWS.getRequestHead req
        mar <- buildAr rhead
        case mar of
            Nothing -> return ()
            Just ar ->
                Y.sendRawResponseNoConduit
                  $ \src sink -> control $ \runInIO -> WaiWS.runWebSockets
                    wsConnOpts
                    rhead
                    (\pconn -> do
                        conn <- WS.acceptRequestWith pconn ar
                        WS.forkPingThread conn 30
                        runInIO $ runReaderT inner conn)
                    src
                    sink

-- | Wrapper for capturing exceptions
wrapWSE :: (MonadIO m, WS.WebSocketsData a) => (WS.Connection -> a -> IO ())-> a -> WebSocketsT m (Either SomeException ())
wrapWSE ws x = ReaderT $ liftIO . tryAny . flip ws x

wrapWS :: (MonadIO m, WS.WebSocketsData a) => (WS.Connection -> a -> IO ()) -> a -> WebSocketsT m ()
wrapWS ws x = ReaderT $ liftIO . flip ws x

-- | Receive a piece of data from the client.
--
-- Since 0.1.0
receiveData :: (MonadIO m, WS.WebSocketsData a) => WebSocketsT m a
receiveData = ReaderT $ liftIO . WS.receiveData

-- | Receive a piece of data from the client.
-- Capture SomeException as the result or operation
-- Since 0.2.2
receiveDataE :: (MonadIO m, WS.WebSocketsData a) => WebSocketsT m (Either SomeException a)
receiveDataE = ReaderT $ liftIO . tryAny . WS.receiveData

-- | Receive an application message.
-- Capture SomeException as the result or operation
-- Since 0.2.3
receiveDataMessageE :: (MonadIO m) => WebSocketsT m (Either SomeException WS.DataMessage)
receiveDataMessageE = ReaderT $ liftIO . tryAny . WS.receiveDataMessage

-- | Send a textual message to the client.
--
-- Since 0.1.0
sendTextData :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendTextData = wrapWS WS.sendTextData

-- | Send a textual message to the client.
-- Capture SomeException as the result or operation
-- and can be used like 
-- `either handle_exception return =<< sendTextDataE ("Welcome" :: Text)`
-- Since 0.2.2
sendTextDataE :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m (Either SomeException ())
sendTextDataE = wrapWSE WS.sendTextData

-- | Send a binary message to the client.
--
-- Since 0.1.0
sendBinaryData :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendBinaryData = wrapWS WS.sendBinaryData

-- | Send a binary message to the client.
-- Capture SomeException as the result of operation
-- Since 0.2.2
sendBinaryDataE :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m (Either SomeException ())
sendBinaryDataE = wrapWSE WS.sendBinaryData

-- | Send a ping message to the client.
--
-- Since 0.2.2
sendPing :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendPing = wrapWS WS.sendPing

-- | Send a ping message to the client. 
-- Capture SomeException as the result of operation
-- Since 0.2.2
sendPingE :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m (Either SomeException ())
sendPingE = wrapWSE WS.sendPing

-- | Send a DataMessage to the client. 
-- Capture SomeException as the result of operation
-- Since 0.2.3
sendDataMessageE :: (MonadIO m) => WS.DataMessage -> WebSocketsT m (Either SomeException ())
sendDataMessageE x = ReaderT $ liftIO . tryAny . (`WS.sendDataMessage` x)

-- | Send a close request to the client. 
-- 
-- Since 0.2.2
sendClose :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendClose = wrapWS WS.sendClose

-- | Send a close request to the client. 
-- Capture SomeException as the result of operation
-- Since 0.2.2
sendCloseE :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m (Either SomeException ())
sendCloseE = wrapWSE WS.sendClose

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
