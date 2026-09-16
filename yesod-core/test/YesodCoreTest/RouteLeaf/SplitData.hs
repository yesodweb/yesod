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
module YesodCoreTest.RouteLeaf.SplitData where

import Data.Text (Text)
import Yesod.Core
import YesodCoreTest.RouteLeaf.SplitResources

-- The top splice must discover and reuse these views, including ChildR's.
mkYesodDataOpts (setFocusOnNestedRoute "ScopeR" splitLeafOpts)
    "SplitLeafApp a" splitLeafResources
