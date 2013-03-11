{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | This module simply re-exports from other modules for your convenience.
module Yesod
    ( -- * Re-exports from yesod-core
      module Yesod.Core
    , module Yesod.Form
    , module Yesod.Persist
      -- * Running your application
    , warp
    , warpDebug
    , warpEnv
    , develServer
      -- * Commonly referenced functions/datatypes
    , Application
    , liftIO
    , MonadBaseControl
      -- * Utilities
    , showIntegral
    , readIntegral
      -- * Hamlet library
      -- ** Hamlet
    , hamlet
    , xhamlet
    , HtmlUrl
    , Html
    , toHtml
      -- ** Julius
    , julius
    , JavascriptUrl
    , renderJavascriptUrl
    , toJSON
      -- ** Cassius/Lucius
    , cassius
    , lucius
    , CssUrl
    , renderCssUrl
    ) where

import Yesod.Core
import Text.Hamlet
import Text.Cassius
import Text.Lucius
import Text.Julius

import Yesod.Form
import Yesod.Persist
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)

import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Handler.Warp (run)
import System.IO (stderr, hPutStrLn)
import Text.Blaze.Html (toHtml)
import System.Environment (getEnv)
import Data.Aeson (toJSON)

showIntegral :: Integral a => a -> String
showIntegral x = show (fromIntegral x :: Integer)

readIntegral :: Num a => String -> Maybe a
readIntegral s =
    case reads s of
        (i, _):_ -> Just $ fromInteger i
        [] -> Nothing

-- | A convenience method to run an application using the Warp webserver on the
-- specified port. Automatically calls 'toWaiApp'.
warp :: (Yesod a, YesodDispatch a a) => Int -> a -> IO ()
warp port a = toWaiApp a >>= run port

-- | Same as 'warp', but also sends a message to stdout for each request, and
-- an \"application launched\" message as well. Can be useful for development.
warpDebug :: (Yesod a, YesodDispatch a a) => Int -> a -> IO ()
warpDebug port app = do
  hPutStrLn stderr $ "Application launched, listening on port " ++ show port
  waiApp <- toWaiApp app
  run port $ logStdout waiApp

-- | Runs your application using default middlewares (i.e., via 'toWaiApp'). It
-- reads port information from the PORT environment variable, as used by tools
-- such as Keter.
--
-- Note that the exact behavior of this function may be modified slightly over
-- time to work correctly with external tools, without a change to the type
-- signature.
warpEnv :: (Yesod a, YesodDispatch a a) => a -> IO ()
warpEnv master = do
    port <- getEnv "PORT" >>= readIO
    app <- toWaiApp master
    run port app

-- | Run a development server, where your code changes are automatically
-- reloaded.
develServer :: Int -- ^ port number
            -> String -- ^ module name holding the code
            -> String -- ^ name of function providing a with-application
            -> IO ()

develServer port modu func =
    mapM_ putStrLn
        [ "Due to issues with GHC 7.0.2, you must now run the devel server"
        , "separately. To do so, ensure you have installed the "
        , "wai-handler-devel package >= 0.2.1 and run:"
        , concat
            [ "   wai-handler-devel "
            , show port
            , " "
            , modu
            , " "
            , func
            , " --yesod"
            ]
        , ""
        ]

