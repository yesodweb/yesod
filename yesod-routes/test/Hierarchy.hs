{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Hierarchy (hierarchy) where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Yesod.Routes.Parse
import Yesod.Routes.TH
import Yesod.Routes.Class
import Language.Haskell.TH.Syntax

data Hierarchy = Hierarchy

mkRenderRouteInstance (ConT ''Hierarchy) $ map (fmap parseType) [parseRoutes|
/ HomeR GET
/admin/#Int AdminR:
    / AdminRootR GET
    /login LoginR GET POST
|]

hierarchy :: Specs
hierarchy = return ()
