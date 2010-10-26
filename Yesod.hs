{-# LANGUAGE CPP #-}
-- | This module simply re-exports from other modules for your convenience.
module Yesod
    ( module Yesod.Request
    , module Yesod.Content
    , module Yesod.Yesod
    , module Yesod.Handler
    , module Yesod.Dispatch
    , module Yesod.Form
    , module Yesod.Hamlet
    , module Yesod.Json
    , module Yesod.Widget
    , Application
    , lift
    , liftIO
    , MonadInvertIO
    , mempty
    , showIntegral
    , readIntegral
    ) where

#if TEST
import Yesod.Content hiding (testSuite)
import Yesod.Json hiding (testSuite)
import Yesod.Dispatch hiding (testSuite)
import Yesod.Yesod hiding (testSuite)
import Yesod.Handler hiding (runHandler, testSuite)
#else
import Yesod.Content
import Yesod.Json
import Yesod.Dispatch
import Yesod.Yesod
import Yesod.Handler hiding (runHandler)
#endif

import Yesod.Request
import Yesod.Form
import Yesod.Widget
import Network.Wai (Application)
import Yesod.Hamlet
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mempty)
import Control.Monad.Invert (MonadInvertIO)

showIntegral :: Integral a => a -> String
showIntegral x = show (fromIntegral x :: Integer)

readIntegral :: Num a => String -> Maybe a
readIntegral s =
    case reads s of
        (i, _):_ -> Just $ fromInteger i
        [] -> Nothing
