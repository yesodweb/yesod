{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | This module simply re-exports from other modules for your convenience.
module Yesod
    ( -- * Re-exports from yesod-core
      module Yesod.Core
    , module Yesod.Form
    , module Yesod.Persist
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
