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
class YesodDispatch sub master where
    yesodDispatch
        :: Yesod master
        => YesodRunnerEnv sub master
        -> W.Application

instance YesodDispatch WaiSubsite master where
    yesodDispatch YesodRunnerEnv { yreSub = WaiSubsite app } req =
        app req

class YesodSubDispatch sub m where
    yesodSubDispatch
        :: (HandlerError m, HandlerState m, master ~ HandlerMaster m, Yesod master, MonadBaseControl IO m)
        => (m TypedContent
                -> YesodRunnerEnv master master
                -> Maybe (Route master)
                -> W.Application)
        -> (master -> sub)
        -> (Route sub -> Route master)
        -> YesodRunnerEnv master master
        -> W.Application

instance YesodSubDispatch WaiSubsite master where
    yesodSubDispatch _ toSub _ YesodRunnerEnv { yreMaster = master } req =
        app req
      where
        WaiSubsite app = toSub master

{-
subHelper :: Yesod master => (YesodRunnerEnv sub master -> W.Application)
        -> (forall res. ToTypedContent res
                => m res
                -> YesodRunnerEnv master master
                -> Maybe (Route master)
                -> W.Application)
        -> (master -> sub)
        -> (Route sub -> Route master)
        -> W.Application
subHelper runBase getSub toMaster = error "subHelper"
-}

subHelper :: (HandlerMaster m ~ master, HandlerState m, MonadBaseControl IO m)
          => (m TypedContent
                -> YesodRunnerEnv master master
                -> Maybe (Route master)
                -> W.Application)
          -> (master -> sub)
          -> (Route sub -> Route master)
          -> HandlerT sub m TypedContent
          -> YesodRunnerEnv master master
          -> Maybe (Route sub)
          -> W.Application
subHelper parentRunner getSub toMaster handlert env route =
    parentRunner base env (fmap toMaster route)
  where
    base = stripHandlerT (fmap toTypedContent handlert) getSub toMaster route
