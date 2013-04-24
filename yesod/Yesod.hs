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
      -- * Utilities
    , showIntegral
    , readIntegral
      -- * Hamlet library
      -- ** Hamlet
    , hamlet
    , xhamlet
    , HtmlUrl
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
import Yesod.Persist

import Network.Wai

showIntegral :: Integral a => a -> String
showIntegral x = show (fromIntegral x :: Integer)

readIntegral :: Num a => String -> Maybe a
readIntegral s =
    case reads s of
        (i, _):_ -> Just $ fromInteger i
        [] -> Nothing
