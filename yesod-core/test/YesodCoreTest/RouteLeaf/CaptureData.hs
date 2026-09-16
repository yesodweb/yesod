{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module YesodCoreTest.RouteLeaf.CaptureData where
import Yesod.Core

data CaptureApp a = CaptureApp
class (Eq a, Show a, Read a, PathPiece a) => Capture a
instance Capture Int

mkYesodDataOpts (setRouteLeafViews True $ setParameterizedSubroute True defaultOpts)
    "(Capture a) => CaptureApp a" [parseRoutes|
/captured/#a CapturedR:
    /value/#a ValueR GET
|]
