{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Yesod.Core.Class.Dispatch where

import Yesod.Core.Content
import Yesod.Core.Handler

import Yesod.Routes.Class

import qualified Network.Wai as W
import Yesod.Core.Internal.Session
import Data.Text (Text)
import System.Log.FastLogger (Logger)
import Yesod.Core.Types
import Yesod.Core.Class.Yesod
import Yesod.Core.Internal.Run

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class YesodDispatch sub master where
    yesodDispatch
        :: Yesod master
        => Logger
        -> master
        -> sub
        -> (Route sub -> Route master)
        -> (Maybe (SessionBackend master) -> W.Application) -- ^ 404 handler
        -> (Route sub -> Maybe (SessionBackend master) -> W.Application) -- ^ 405 handler
        -> Text -- ^ request method
        -> [Text] -- ^ pieces
        -> Maybe (SessionBackend master)
        -> W.Application

    yesodRunner :: Yesod master
                => Logger
                -> GHandler sub master TypedContent
                -> master
                -> sub
                -> Maybe (Route sub)
                -> (Route sub -> Route master)
                -> Maybe (SessionBackend master)
                -> W.Application
    yesodRunner logger handler master sub murl tomaster msb = defaultYesodRunner YesodRunnerEnv
        { yreLogger = logger
        , yreMaster = master
        , yreSub = sub
        , yreRoute = murl
        , yreToMaster = tomaster
        , yreSessionBackend = msb
        } handler

instance YesodDispatch WaiSubsite master where
    yesodDispatch _logger _master (WaiSubsite app) _tomaster _404 _405 _method _pieces _session = app
