{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Data module for the split-subsite test. Defines a (non-parameterized)
-- subsite with a nested route, and generates its route datatype, render and
-- parse instances. Because the subsite has no type parameters, the
-- nested-discovery machinery (RenderRouteNested / ParseRouteNested for the
-- nested 'NestedR' fragment) is generated here, so that a separately compiled
-- module can supply the matching 'YesodSubDispatchNested' instance.
module YesodCoreTest.SplitSubsite.Data where

import Yesod.Core

data SplitSub = SplitSub

mkYesodSubData "SplitSub" [parseRoutes|
/ SplitHomeR GET
/nested NestedR:
    / NestedHomeR GET
    /detail/#Int NestedDetailR GET
|]
