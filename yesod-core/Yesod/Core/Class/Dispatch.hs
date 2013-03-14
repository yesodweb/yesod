{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts  #-}
module Yesod.Core.Class.Dispatch where

import Yesod.Routes.Class
import qualified Network.Wai as W
import Yesod.Core.Types
import Yesod.Core.Content
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Handler
import Yesod.Core.Internal.Request (textQueryString)
import Yesod.Core.Internal.Run
import           Control.Monad.Trans.Control  (MonadBaseControl)

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class Yesod site => YesodDispatch site where
    yesodDispatch :: YesodRunnerEnv site -> W.Application

class YesodSubDispatch sub m where
    yesodSubDispatch
        :: (m TypedContent
                -> YesodRunnerEnv (HandlerSite m)
                -> Maybe (Route (HandlerSite m))
                -> W.Application)
        -> (HandlerSite m -> sub)
        -> (Route sub -> Route (HandlerSite m))
        -> YesodRunnerEnv (HandlerSite m)
        -> W.Application

instance YesodSubDispatch WaiSubsite master where
    yesodSubDispatch _ toSub _ YesodRunnerEnv { yreSite = site } req =
        app req
      where
        WaiSubsite app = toSub site

-- | A helper function for creating YesodSubDispatch instances, used by the
-- internal generated code.
subHelper :: Monad m
          => (HandlerT parent m TypedContent
                -> YesodRunnerEnv parent
                -> Maybe (Route parent)
                -> W.Application)
          -> (parent -> child)
          -> (Route child -> Route parent)
          -> HandlerT child (HandlerT parent m) TypedContent
          -> YesodRunnerEnv parent
          -> Maybe (Route child)
          -> W.Application
subHelper parentRunner getSub toMaster handlert env route =
    parentRunner base env (fmap toMaster route)
  where
    base = stripHandlerT (fmap toTypedContent handlert) getSub toMaster route
