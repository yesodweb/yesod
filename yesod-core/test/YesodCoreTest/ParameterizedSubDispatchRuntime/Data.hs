{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Route data for the runtime opt-out coverage in
-- "YesodCoreTest.ParameterizedSubDispatchRuntime". Kept in its own module so
-- the 'mkYesodSubDispatch' splices there can refer to the @resources*@ values
-- generated here (TH stage restriction).
module YesodCoreTest.ParameterizedSubDispatchRuntime.Data where

import Yesod.Core
import Data.Text (Text)

-- | A monomorphic leaf subsite, embedded under a 'ResourceParent' of 'BigSub'
-- to exercise the inline @Subsite@ arm
-- ('generateParseRouteClausesInline', ParseRoute.hs:234).
data ChildSub = ChildSub

mkYesodSubData "ChildSub" [parseRoutes|
/ ChildHomeR GET
/x/#Int ChildXR GET
|]

-- | A parameterized (hence opt-out) subsite that covers a multipiece leaf and
-- a subsite leaf nested under a 'ResourceParent'. The type parameter @a@ is
-- phantom — its only job is to make @tyargs@ non-empty so the
-- backwards-compatible inline path is taken.
newtype BigSub a = BigSub a

mkYesodSubData "BigSub a" [parseRoutes|
/item/#Int BigItemR GET
/multi/+[Text] BigMultiR GET
/wrap/#Int WrapR:
    / WrapHomeR GET
    /detail/#Int WrapDetailR GET
/embed EmbedParentR:
    /sub ChildR ChildSub getChildSub
|]

getChildSub :: BigSub a -> ChildSub
getChildSub _ = ChildSub
