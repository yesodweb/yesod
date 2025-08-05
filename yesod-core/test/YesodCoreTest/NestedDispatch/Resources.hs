{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.NestedDispatch.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types

data App = App

nestedDispatchResources :: [ResourceTree String]
nestedDispatchResources = [parseRoutes|
/     HomeR GET !home
/json JsonR GET
/nest NestR:
    / NestIndexR GET POST

/parent/#Int ParentR:
    /#Text/child1 Child1R !child
    /#Int/child2 Child2R !child GET POST
|]

