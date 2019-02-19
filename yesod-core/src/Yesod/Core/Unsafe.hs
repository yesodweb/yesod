{-# LANGUAGE NoImplicitPrelude #-}
-- | This is designed to be used as
--
-- > import qualified Yesod.Core.Unsafe as Unsafe
--
-- This serves as a reminder that the functions are unsafe to use in many situations.
module Yesod.Core.Unsafe (runFakeHandler, fakeHandlerGetLogger) where

import RIO
import Yesod.Core.Internal.Run (runFakeHandler)

import Yesod.Core.Types
import Yesod.Core.Class.Yesod

-- | designed to be used as
--
-- > unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
fakeHandlerGetLogger :: (Yesod site, MonadIO m)
                     => LogFunc
                     -> site
                     -> HandlerFor site a
                     -> m a
fakeHandlerGetLogger logFunc app f =
     runFakeHandler mempty logFunc app f
 >>= either (error . ("runFakeHandler issue: " `mappend`) . show)
            return
