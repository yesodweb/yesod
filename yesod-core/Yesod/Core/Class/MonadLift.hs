{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Yesod.Core.Class.MonadLift (MonadLift (..)) where

import Control.Monad.Trans.Class

-- | The standard @MonadTrans@ class only allows lifting for monad
-- transformers. While @GHandler@ and @GWidget@ should allow lifting, their
-- types do not express that they actually are transformers. This replacement
-- class accounts for this.
class MonadLift base m | m -> base where
    lift :: base a -> m a
instance (Monad m, MonadTrans t) => MonadLift m (t m) where
    lift = Control.Monad.Trans.Class.lift
