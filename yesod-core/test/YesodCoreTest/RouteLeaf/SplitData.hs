{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module YesodCoreTest.RouteLeaf.SplitData
    ( ScopeR (..), GroupR (..), ChildR (..), RouteLeaves (LeafLocalR) ) where

import Data.Text (Text)
import Yesod.Core
import Yesod.Core.RouteLeaf (RouteLeaves)
import YesodCoreTest.RouteLeaf.SplitResources

-- The top splice must reuse ChildR's view without importing its leaf constructor.
mkYesodDataOpts (setFocusOnNestedRoute "ScopeR" splitLeafOpts)
    "SplitLeafApp a" splitLeafResources
