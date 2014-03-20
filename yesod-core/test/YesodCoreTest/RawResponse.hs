{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables #-}
module YesodCoreTest.RawResponse (specs, Widget) where

import Yesod.Core
import Test.Hspec
import qualified Data.Map as Map
import Network.Wai.Test
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

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
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

getFreePort :: IO Int
getFreePort = do
    loop 43124
  where
    loop port = do
        esocket <- try $ bindPort port "*"
        case esocket of
            Left (_ :: IOException) -> loop (succ port)
            Right socket -> do
                sClose socket
                return port

specs :: Spec
specs = describe "RawResponse" $ do
    it "works" $ do
        port <- getFreePort
        withAsync (warp port App) $ \_ -> do
            threadDelay 100000
            runTCPClient (clientSettings port "127.0.0.1") $ \ad -> do
                yield "GET / HTTP/1.1\r\n\r\nhello" $$ appSink ad
                (appSource ad $$ CB.take 6) >>= (`shouldBe` "0HELLO")
                yield "WORLd" $$ appSink ad
                (appSource ad $$ await) >>= (`shouldBe` Just "WORLD")
