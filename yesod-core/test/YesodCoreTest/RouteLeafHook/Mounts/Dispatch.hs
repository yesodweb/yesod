{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module YesodCoreTest.RouteLeafHook.Mounts.Dispatch () where

import Yesod.Core
-- Deliberately hide RouteLeaves constructors across the data/dispatch boundary.
import YesodCoreTest.RouteLeafHook.Mounts.Data
    (MountApp, MountSub, WrongRouteSub, MountGroupR (..), Route (..), resourcesMountApp,
     getRootSub, getNestedSub, getWrongSub, recordMountEvent)
import YesodCoreTest.RouteLeafHook.Mounts.Policy ()
import YesodCoreTest.RouteLeafHook.Options

mkYesodDispatchOpts
    (setFocusOnNestedRoute "MountGroupR" $ setParameterizedSubroute True hookRouteOpts)
    "MountApp a" resourcesMountApp

mkYesodDispatchOpts (setParameterizedSubroute True hookRouteOpts)
    "MountApp a" resourcesMountApp

getPlainR :: HandlerFor (MountApp a) String
getPlainR = recordMountEvent "handler" >> pure "plain"
