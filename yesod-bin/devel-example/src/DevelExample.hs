{-# LANGUAGE OverloadedStrings #-}
module DevelExample
    ( prodMain
    , develMain
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Directory (doesFileExist)
import System.Environment

myApp :: Application
myApp _req send = send $ responseLBS
    status200
    [(hContentType, "text/html; charset=utf-8")]
    "<p>Well, this is really <b>boring</b>.</p>"

prodMain :: IO ()
prodMain = do
    putStrLn "Running in production mode on port 8080"
    run 8080 $ logStdout myApp

develMain :: IO ()
develMain = race_ watchTermFile $ do
    port <- fmap read $ getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putStrLn $ "Running in development mode on port " ++ show port
    putStrLn $ "But you should connect to port " ++ displayPort
    run port $ logStdoutDev myApp

-- | Would certainly be more efficient to use fsnotify, but this is
-- simpler.
watchTermFile :: IO ()
watchTermFile =
    loop
  where
    loop = do
        exists <- doesFileExist "yesod-devel/devel-terminate"
        if exists
            then return ()
            else do
                threadDelay 100000
                loop
