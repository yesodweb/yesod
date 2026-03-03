{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
-- ^ Useful for debugging generated code; can be removed once stable

-- | Test case for mkYesodSubData with parameterized/constrained types.
-- Reproduces the bug reported by l0neGamer on PR #1887:
-- mkYesodSubData with a constrained type like
--   @"(SomeClass subsite master) => SubsiteData subsite"@
-- where 'master' appears in the context but NOT as a type parameter of
-- the subsite, produces "Out of scope type variable" errors in family
-- instances.
module YesodCoreTest.ParameterizedSubData where

import Yesod.Core

-- | A multi-param type class where 'master' is NOT a parameter of the
-- subsite type, mimicking the pattern:
--   @mkYesodSubData "(SubsiteClass subsite master) => SubsiteData subsite"@
-- The fundep ensures 'master' is determined by 'subsite', which is
-- typical for real-world subsite classes.
class ParamSubsiteClass subsite master | subsite -> master where
  getSubsiteValue :: subsite -> master -> String

data ParamSubsite subsite = ParamSubsite subsite

mkYesodSubData "(ParamSubsiteClass subsite master) => ParamSubsite subsite" [parseRoutes|
/ ParamSubHomeR GET
/item/#Int ParamSubItemR GET
/nested NestedR:
    / NestedHomeR GET
    /detail/#Int NestedDetailR GET
|]
