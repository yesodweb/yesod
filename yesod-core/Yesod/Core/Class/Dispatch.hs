{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Yesod.Core.Class.Dispatch where

import Yesod.Routes.Class
import qualified Network.Wai as W
import Yesod.Core.Types
import Yesod.Core.Class.Yesod
import Yesod.Core.Internal.Request (textQueryString)

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class YesodDispatch sub master where
    yesodDispatch
        :: Yesod master
        => YesodRunnerEnv sub master
        -> W.Application

instance YesodDispatch WaiSubsite master where
    yesodDispatch YesodRunnerEnv { yreSub = WaiSubsite app } req =
        app req
