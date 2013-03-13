module Yesod.Core.Types.Orphan where

import Yesod.Core.Types
import Control.Monad.Trans.Class

instance MonadTrans (HandlerT sub) where
    lift = HandlerT . const
