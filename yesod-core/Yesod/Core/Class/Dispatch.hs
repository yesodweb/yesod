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
        => W.Application -- ^ 404 handler
        -> (Route sub -> W.Application) -- ^ 405 handler
        -> (Route sub -> YesodRunnerEnv sub master)
        -> W.Application

instance YesodDispatch WaiSubsite master where
    yesodDispatch _404 _405 getEnv req =
        app req
      where
        YesodRunnerEnv { yreSub = WaiSubsite app } = getEnv $ WaiSubsiteRoute (W.pathInfo req) (textQueryString req)
