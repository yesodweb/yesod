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
    ) where

import           Control.Monad                  (when, forever)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Control    (control)
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
        Y.sendRawResponse $ \src sink -> control $ \runInIO -> WaiWS.runWebSockets
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

-- | Send a textual messsage to the client.
--
-- Since 0.1.0
sendTextData :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendTextData x = ReaderT $ liftIO . flip WS.sendTextData x

-- | Send a binary messsage to the client.
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
