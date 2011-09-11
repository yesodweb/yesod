{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | This module simply re-exports from other modules for your convenience.
module Yesod
    ( -- * Re-exports from yesod-core
      module Yesod.Core
    , module Yesod.Form
    , module Yesod.Json
    , module Yesod.Persist
    , module Yesod.Settings
    , module Yesod.Main
      -- * Running your application
    , warp
    , warpDebug
    , develServer
      -- * Commonly referenced functions/datatypes
    , Application
    , lift
    , liftIO
    , MonadControlIO
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
import Yesod.Json
import Yesod.Persist
import Yesod.Settings
import Yesod.Main
import Network.Wai (Application)
import Network.Wai.Middleware.Debug
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Control (MonadControlIO)

import Network.Wai.Handler.Warp (run)
import System.IO (stderr, hPutStrLn)
import Text.Blaze (toHtml)

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

-- | Same as 'warp', but also sends a message to stderr for each request, and
-- an \"application launched\" message as well. Can be useful for development.
warpDebug :: (Yesod a, YesodDispatch a a) => Int -> a -> IO ()
warpDebug port a = do
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    toWaiApp a >>= run port . debug

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

