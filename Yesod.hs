{-# LANGUAGE CPP #-}
-- | This module simply re-exports from other modules for your convenience.
module Yesod
    ( -- * Re-exports from yesod-core
      module Yesod.Request
    , module Yesod.Content
    , module Yesod.Core
    , module Yesod.Handler
    , module Yesod.Dispatch
    , module Yesod.Widget
      -- * Commonly referenced functions/datatypes
    , Application
    , lift
    , liftIO
    , MonadPeelIO
      -- * Utilities
    , showIntegral
    , readIntegral
      -- * Hamlet library
      -- ** Hamlet
    , hamlet
    , xhamlet
    , Hamlet
    , Html
    , renderHamlet
    , renderHtml
    , string
    , preEscapedString
    , cdata
      -- ** Julius
    , julius
    , Julius
    , renderJulius
      -- ** Cassius
    , cassius
    , Cassius
    , renderCassius
    ) where

import Yesod.Content
import Yesod.Dispatch
import Yesod.Core
import Yesod.Handler hiding (runHandler)
import Text.Hamlet
import Text.Cassius
import Text.Julius

import Yesod.Request
import Yesod.Widget
import Network.Wai (Application)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Peel (MonadPeelIO)

showIntegral :: Integral a => a -> String
showIntegral x = show (fromIntegral x :: Integer)

readIntegral :: Num a => String -> Maybe a
readIntegral s =
    case reads s of
        (i, _):_ -> Just $ fromInteger i
        [] -> Nothing
