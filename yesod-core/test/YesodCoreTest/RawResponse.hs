{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables #-}
module YesodCoreTest.RawResponse (specs, Widget) where

import Yesod.Core
import Test.Hspec
import qualified Data.Map as Map
import Network.Wai.Test
import Network.Wai (responseStream)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as S8
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Char (toUpper)
import Control.Exception (try, IOException)
import Data.Conduit.Network
import Network.Socket (sClose)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad.Trans.Resource (register)
import Data.IORef
import Data.Streaming.Network (bindPortTCP)
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (fromByteString)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/wai-stream WaiStreamR GET
/wai-app-stream WaiAppStreamR GET
|]

instance Yesod App

getHomeR :: Handler ()
getHomeR = do
    ref <- liftIO $ newIORef 0
    _ <- register $ writeIORef ref 1
    sendRawResponse $ \src sink -> liftIO $ do
        val <- readIORef ref
        yield (S8.pack $ show val) $$ sink
        src $$ CL.map (S8.map toUpper) =$ sink

getWaiStreamR :: Handler ()
getWaiStreamR = sendWaiResponse $ responseStream status200 [] $ \send flush -> do
    flush
    send $ fromByteString "hello"
    flush
    send $ fromByteString " world"

getWaiAppStreamR :: Handler ()
getWaiAppStreamR = sendWaiApplication $ \_ f -> f $ responseStream status200 [] $ \send flush -> do
    flush
    send $ fromByteString "hello"
    flush
    send $ fromByteString " world"

getFreePort :: IO Int
getFreePort = do
    loop 43124
  where
    loop port = do
        esocket <- try $ bindPortTCP port "*"
        case esocket of
            Left (_ :: IOException) -> loop (succ port)
            Right socket -> do
                sClose socket
                return port

specs :: Spec
specs = do
    describe "RawResponse" $ do
        it "works" $ do
            port <- getFreePort
            withAsync (warp port App) $ \_ -> do
                threadDelay 100000
                runTCPClient (clientSettings port "127.0.0.1") $ \ad -> do
                    yield "GET / HTTP/1.1\r\n\r\nhello" $$ appSink ad
                    (appSource ad $$ CB.take 6) >>= (`shouldBe` "0HELLO")
                    yield "WORLd" $$ appSink ad
                    (appSource ad $$ await) >>= (`shouldBe` Just "WORLD")

    let body req = do
            port <- getFreePort
            withAsync (warp port App) $ \_ -> do
                threadDelay 100000
                runTCPClient (clientSettings port "127.0.0.1") $ \ad -> do
                    yield req $$ appSink ad
                    appSource ad $$ CB.lines =$ do
                        let loop = do
                                x <- await
                                case x of
                                    Nothing -> return ()
                                    Just "\r" -> return ()
                                    _ -> loop
                        loop

                        Just "0005\r" <- await
                        Just "hello\r" <- await

                        Just "0006\r" <- await
                        Just " world\r" <- await

                        return ()
    it "sendWaiResponse + responseStream" $ do
        body "GET /wai-stream HTTP/1.1\r\n\r\n"
    it "sendWaiApplication + responseStream" $ do
        body "GET /wai-app-stream HTTP/1.1\r\n\r\n"
