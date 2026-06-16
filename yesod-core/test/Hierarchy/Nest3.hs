{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

-- | @Hierarchy@ split fragment focused on the @Nest3@ parent. Demonstrates
-- @mkYesodDataOpts@ with @setFocusOnNestedRoute "Nest3"@ emitting just that
-- nested fragment's route datatype + instances, separate from the whole-tree
-- splice in "Hierarchy". Shares @hierarchyResources@ from "Hierarchy.ResourceTree".
module Hierarchy.Nest3 where

import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Yesod.Core

mkYesodDataOpts (setFocusOnNestedRoute "Nest3" defaultOpts) "Hierarchy" hierarchyResources
