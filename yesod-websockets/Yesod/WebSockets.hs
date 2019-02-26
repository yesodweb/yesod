{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Yesod.WebSockets
    ( -- * Core API
      webSockets
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

import           Control.Monad                  (forever, when)
import           Control.Monad.Reader           (ReaderT, runReaderT, MonadReader, ask)
import           Conduit
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import qualified Yesod.Core                     as Y
import           RIO

-- FIXME document
class Y.HasHandlerData env => HasWebsockets env where
  websocketsL :: Lens' env WS.Connection

data WithWebsockets env = WithWebsockets
  { wwConnection :: !WS.Connection
  , wwEnv :: !env
  }

-- | Attempt to run a WebSockets handler. This function first checks if the
-- client initiated a WebSockets connection and, if so, runs the provided
-- application, short-circuiting the rest of your handler. If the client did
-- not request a WebSockets connection, the rest of your handler will be called
-- instead.
--
-- Since 0.1.0
webSockets
  :: Y.HasHandlerData env
  => RIO (WithWebsockets env) ()
  -> RIO env ()
webSockets = webSocketsOptions WS.defaultConnectionOptions

-- | Varient of 'webSockets' which allows you to specify
-- the WS.ConnectionOptions setttings when upgrading to a websocket connection.
--
-- Since 0.2.5
webSocketsOptions
  :: Y.HasHandlerData env
  => WS.ConnectionOptions
  -> RIO (WithWebsockets env) ()
  -> RIO env ()
webSocketsOptions opts = webSocketsOptionsWith opts $ const $ return $ Just $ WS.AcceptRequest Nothing []

-- | Varient of 'webSockets' which allows you to specify the 'WS.AcceptRequest'
-- setttings when upgrading to a websocket connection.
--
-- Since 0.2.4
webSocketsWith :: Y.HasHandlerData env
               => (WS.RequestHead -> RIO env (Maybe WS.AcceptRequest))
               -- ^ A Nothing indicates that the websocket upgrade request should not happen
               -- and instead the rest of the handler will be called instead.  This allows
               -- you to use 'WS.getRequestSubprotocols' and only accept the request if
               -- a compatible subprotocol is given.  Also, the action runs before upgrading
               -- the request to websockets, so you can also use short-circuiting handler
               -- actions such as 'Y.invalidArgs'.
               -> RIO (WithWebsockets env) ()
               -> RIO env ()
webSocketsWith = webSocketsOptionsWith WS.defaultConnectionOptions

-- | Varient of 'webSockets' which allows you to specify both
-- the WS.ConnectionOptions and the 'WS.AcceptRequest'
-- setttings when upgrading to a websocket connection.
--
-- Since 0.2.5
webSocketsOptionsWith :: Y.HasHandlerData env
               => WS.ConnectionOptions
               -- ^ Custom websockets options
               -> (WS.RequestHead -> RIO env (Maybe WS.AcceptRequest))
               -- ^ A Nothing indicates that the websocket upgrade request should not happen
               -- and instead the rest of the handler will be called instead.  This allows
               -- you to use 'WS.getRequestSubprotocols' and only accept the request if
               -- a compatible subprotocol is given.  Also, the action runs before upgrading
               -- the request to websockets, so you can also use short-circuiting handler
               -- actions such as 'Y.invalidArgs'.
               -> RIO (WithWebsockets env) ()
               -> RIO env ()
webSocketsOptionsWith wsConnOpts buildAr inner = do
    req <- Y.waiRequest
    when (WaiWS.isWebSocketsReq req) $ do
        let rhead = WaiWS.getRequestHead req
        mar <- buildAr rhead
        case mar of
            Nothing -> return ()
            Just ar -> do
                env <- ask
                Y.sendRawResponseNoConduit
                  $ \src sink -> liftIO $ WaiWS.runWebSockets
                    wsConnOpts
                    rhead
                    (\pconn -> do
                        conn <- WS.acceptRequestWith pconn ar
                        WS.forkPingThread conn 30
                        let ww = WithWebsockets conn env
                        runRIO ww inner)
                    src
                    sink

-- | Wrapper for capturing exceptions
wrapWSE :: HasWebsockets env
        => (WS.Connection -> a -> IO ())
        -> a
        -> RIO env (Either SomeException ())
wrapWSE ws x = do
  conn <- view websocketsL
  liftIO $ tryAny $ ws conn x

wrapWS :: HasWebsockets env
       => (WS.Connection -> a -> IO ())
       -> a
       -> RIO env ()
wrapWS ws x = do
  conn <- view websocketsL
  liftIO $ ws conn x

-- | Receive a piece of data from the client.
--
-- Since 0.1.0
receiveData
  :: (WS.WebSocketsData a, HasWebsockets env)
  => RIO env a
receiveData = do
  conn <- view websocketsL
  liftIO $ WS.receiveData conn

-- | Receive a piece of data from the client.
-- Capture SomeException as the result or operation
-- Since 0.2.2
receiveDataE
  :: (MonadIO m, MonadReader WS.Connection m, WS.WebSocketsData a)
  => m (Either SomeException a)
receiveDataE = do
  conn <- ask
  liftIO $ tryAny $ WS.receiveData conn

-- | Receive an application message.
-- Capture SomeException as the result or operation
-- Since 0.2.3
receiveDataMessageE
  :: (MonadIO m, MonadReader WS.Connection m)
  => m (Either SomeException WS.DataMessage)
receiveDataMessageE = do
  conn <- ask
  liftIO $ tryAny $ WS.receiveDataMessage conn

-- | Send a textual message to the client.
--
-- Since 0.1.0
sendTextData
  :: (WS.WebSocketsData a, HasWebsockets env)
  => a
  -> RIO env ()
sendTextData = wrapWS WS.sendTextData

-- | Send a textual message to the client.
-- Capture SomeException as the result or operation
-- and can be used like
-- `either handle_exception return =<< sendTextDataE ("Welcome" :: Text)`
-- Since 0.2.2
sendTextDataE
  :: (WS.WebSocketsData a, HasWebsockets env)
  => a
  -> RIO env (Either SomeException ())
sendTextDataE = wrapWSE WS.sendTextData

-- | Send a binary message to the client.
--
-- Since 0.1.0
sendBinaryData
  :: (WS.WebSocketsData a, HasWebsockets env)
  => a
  -> RIO env ()
sendBinaryData = wrapWS WS.sendBinaryData

-- | Send a binary message to the client.
-- Capture SomeException as the result of operation
-- Since 0.2.2
sendBinaryDataE
  :: (WS.WebSocketsData a, HasWebsockets env)
  => a
  -> RIO env (Either SomeException ())
sendBinaryDataE = wrapWSE WS.sendBinaryData

-- | Send a ping message to the client.
--
-- Since 0.2.2
sendPing
  :: (WS.WebSocketsData a, HasWebsockets env)
  => a
  -> RIO env ()
sendPing = wrapWS WS.sendPing

-- | Send a ping message to the client.
-- Capture SomeException as the result of operation
-- Since 0.2.2
sendPingE
  :: (WS.WebSocketsData a, HasWebsockets env)
  => a
  -> RIO env (Either SomeException ())
sendPingE = wrapWSE WS.sendPing

-- | Send a DataMessage to the client.
-- Capture SomeException as the result of operation
-- Since 0.2.3
sendDataMessageE
  :: (MonadIO m, MonadReader WS.Connection m)
  => WS.DataMessage
  -> m (Either SomeException ())
sendDataMessageE x = do
  conn <- ask
  liftIO $ tryAny $ WS.sendDataMessage conn x

-- | Send a close request to the client.
--
-- Since 0.2.2
sendClose
  :: (WS.WebSocketsData a, HasWebsockets env)
  => a
  -> RIO env ()
sendClose = wrapWS WS.sendClose

-- | Send a close request to the client.
-- Capture SomeException as the result of operation
-- Since 0.2.2
sendCloseE
  :: (WS.WebSocketsData a, HasWebsockets env)
  => a
  -> RIO env (Either SomeException ())
sendCloseE = wrapWSE WS.sendClose

-- | A @Source@ of WebSockets data from the user.
--
-- Since 0.1.0
sourceWS
  :: (WS.WebSocketsData a, HasWebsockets env)
  => ConduitT i a (RIO env) ()
sourceWS = forever $ lift receiveData >>= yield

-- | A @Sink@ for sending textual data to the user.
--
-- Since 0.1.0
sinkWSText
  :: (WS.WebSocketsData a, HasWebsockets env)
  => ConduitT a o (RIO env) ()
sinkWSText = mapM_C sendTextData

-- | A @Sink@ for sending binary data to the user.
--
-- Since 0.1.0
sinkWSBinary
  :: (WS.WebSocketsData a, HasWebsockets env)
  => ConduitT a o (RIO env) ()
sinkWSBinary = mapM_C sendBinaryData
