{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Application.Devel
    ( -- * Types
      AppHolder
    , AppRunner
    , WithAppRunner
      -- * Functions
    , initAppHolder
    , swapApp
    , swapAppSimple
    , toApp
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
    ( MVar, newEmptyMVar, newMVar
    , takeMVar, putMVar, readMVar
    )
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status500)
import Data.ByteString.Lazy.Char8 ()
import Control.Monad.IO.Class (liftIO)

type AppHolder = MVar (Application, MVar ())
type AppRunner = Application -> IO ()
type WithAppRunner = AppRunner -> IO ()

initAppHolder :: IO AppHolder
initAppHolder = do
    flag <- newEmptyMVar
    newMVar (initApp, flag)
  where
    initApp _ = return
              $ responseLBS status500 [("Content-Type", "text/plain")]
              $ "No app has yet been loaded"

swapAppSimple :: Application -> AppHolder -> IO ()
swapAppSimple app =
    swapApp war
  where
    war f = f app

swapApp :: WithAppRunner -> AppHolder -> IO ()
swapApp war ah = void $ forkIO $ war $ \app -> do
    (_, oldFlag) <- takeMVar ah
    -- allow the old app to cleanup
    putMVar oldFlag ()
    -- now place the new app into the AppHolder, waiting for a termination
    -- signal
    flag <- newEmptyMVar
    putMVar ah (app, flag)
    takeMVar flag -- this causes execution to hang until we are terminated
  where
    void x = x >> return ()

toApp :: AppHolder -> Application
toApp ah req = do
    (app, _) <- liftIO $ readMVar ah
    app req
