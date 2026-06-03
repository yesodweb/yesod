{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test mkYesodSubDispatch with parameterized/constrained types.
-- This exercises the Dispatch TH code path for parameterized subsites.
--
-- Tests two subsites:
-- 1. ParamSubDispatch (from Data submodule) - simple parameterized subsite
-- 2. ParamSubsite (from ParameterizedSubData) - with associated types,
--    dynamic pieces in nested route parents, and ConstraintKinds
module YesodCoreTest.ParameterizedSubDispatch where

import Yesod.Core
import Data.Text (Text)
import YesodCoreTest.ParameterizedSubDispatch.Data
import YesodCoreTest.ParameterizedSubData

-- Handlers for ParamSubDispatch (simple parameterized subsite)
getParamDispHomeR :: SubHandlerFor (ParamSubDispatch subsite) master Text
getParamDispHomeR = pure "home"

getParamDispItemR :: Int -> SubHandlerFor (ParamSubDispatch subsite) master Text
getParamDispItemR _ = pure "item"

instance ParamSubDispatchClass subsite master => YesodSubDispatch (ParamSubDispatch subsite) master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesParamSubDispatch)

-- Handlers for ParamSubsite (with associated types and nested routes)
getParamSubHomeR :: SubHandlerFor (ParamSubsite subsite) master Text
getParamSubHomeR = pure "paramSubHome"

getParamSubItemR :: Int -> SubHandlerFor (ParamSubsite subsite) master Text
getParamSubItemR _ = pure "paramSubItem"

getNestedHomeR :: AssocType subsite -> SubHandlerFor (ParamSubsite subsite) master Text
getNestedHomeR _ = pure "nestedHome"

getNestedDetailR :: AssocType subsite -> Int -> SubHandlerFor (ParamSubsite subsite) master Text
getNestedDetailR _ _ = pure "nestedDetail"

instance
  ( ParamSubsiteClass subsite master
  ) => YesodSubDispatch (ParamSubsite subsite) master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesParamSubsite)

-- | Backwards-compatibility guard for the reported regression.
--
-- By default ('defaultOpts', as used by 'mkYesodSubData'), nested subroute
-- datatypes must remain *unparameterized* — kind 'Type' — exactly as
-- pre-nested-discovery Yesod generated them. The reported breakage was that
-- the branch silently gave them the parent's type parameter (kind
-- @Type -> Type@), which broke any downstream type signature mentioning the
-- subroute.
--
-- This signature only compiles if 'NestedR' has kind 'Type'. If the default
-- ever regresses to parameterizing subroutes, 'NestedR' would have kind
-- @Type -> Type@ and this would fail with a kind error — turning the
-- downstream breakage into a local, in-repo test failure.
_nestedRArityGuard :: NestedR -> NestedR
_nestedRArityGuard = id
