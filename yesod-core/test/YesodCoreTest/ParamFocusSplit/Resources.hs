{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Shared route tree + foundation datatype for the parameterized
-- focus-split fixture ("YesodCoreTest.ParamFocusSplit" and its
-- @.SubR@ fragment module). The foundation @PApp a@ carries one type
-- argument, so this drives the @TyArgs@-applying focused codegen paths.
module YesodCoreTest.ParamFocusSplit.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types (ResourceTree)

-- | A phantom-parameterized top-level site (kind @Type -> Type@).
data PApp a = PApp

paramFocusResources :: [ResourceTree String]
paramFocusResources = [parseRoutes|
/ HomeR GET
/item/#Int ItemR GET
/sub SubR:
    / SubHomeR GET
    /detail/#Int SubDetailR GET
|]
