{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables #-}
module YesodCoreTest.RawResponse
    ( specs
    , Widget
    , resourcesApp
    ) where

import Yesod.Core
import Test.Hspec
import Network.Wai (responseStream)
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as S8
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Char (toUpper)
import Data.Conduit.Network
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad.Trans.Resource (register)
import Data.IORef
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (testWithApplication)

mkYesod "App" [parseRoutes|
/ HomeR GET
/wai-stream WaiStreamR GET
/wai-app-stream WaiAppStreamR GET
|]

data App = App

instance Yesod App

getHomeR :: Handler ()
getHomeR = do
    ref <- liftIO $ newIORef (0 :: Int)
    _ <- register $ writeIORef ref 1
    sendRawResponse $ \src sink -> liftIO $ do
        val <- readIORef ref
        runConduit $ yield (S8.pack $ show val) .| sink
        runConduit $ src .| CL.map (S8.map toUpper) .| sink

getWaiStreamR :: Handler ()
getWaiStreamR = sendWaiResponse $ responseStream status200 [] $ \send flush -> do
    flush
    send "hello"
    flush
    send " world"

getWaiAppStreamR :: Handler ()
getWaiAppStreamR = sendWaiApplication $ \_ f -> f $ responseStream status200 [] $ \send flush -> do
    flush
    send "hello"
    flush
    send " world"

allowFiveSeconds :: IO a -> IO a
allowFiveSeconds = fmap (either id id) . race (threadDelay 5000000 >> error "timed out")

specs :: Spec
specs = do
    describe "RawResponse" $ do
        it "works" $ allowFiveSeconds $ testWithApplication (toWaiApp App) $ \port -> do
            runTCPClient (clientSettings port "127.0.0.1") $ \ad -> do
                runConduit $ yield "GET / HTTP/1.1\r\n\r\nhello" .| appSink ad
                runConduit (appSource ad .| CB.take 6) >>= (`shouldBe` "0HELLO")
                runConduit $ yield "WORLd" .| appSink ad
                runConduit (appSource ad .| await) >>= (`shouldBe` Just "WORLD")

    let body req = allowFiveSeconds $ testWithApplication (toWaiApp App) $ \port -> do
            runTCPClient (clientSettings port "127.0.0.1") $ \ad -> do
                runConduit $ yield req .| appSink ad
                runConduit $ appSource ad .| CB.lines .| do
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
