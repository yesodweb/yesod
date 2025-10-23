{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}

module Hierarchy.Nest3 where

import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Yesod.Core

mkYesodDataOpts (setFocusOnNestedRoute (Just "Nest3") defaultOpts) "Hierarchy" hierarchyResources
