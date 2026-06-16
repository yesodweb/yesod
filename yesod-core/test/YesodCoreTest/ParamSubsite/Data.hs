{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Opt-in counterpart to "YesodCoreTest.ParameterizedSubData": this subsite
-- enables parameterized subroutes via
-- @'mkYesodSubDataOpts' ('setParameterizedSubroute' 'True' ...)@. With the
-- flag on, the nested subroute datatype 'PNestedR' carries the subsite's type
-- parameter (kind @Type -> Type@), so the threaded constraints / associated
-- type families stay well-scoped. This proves that opting a /subsite/ into
-- parameterized subroutes actually parameterizes its subroutes.
module YesodCoreTest.ParamSubsite.Data where

import Yesod.Core

type RouteConstraints t = (Eq t, Read t, Show t, PathPiece t)

class (RouteConstraints (PAssoc subsite)) => PClass subsite master | subsite -> master where
  type PAssoc subsite
  pValue :: subsite -> master -> String

newtype PSub subsite = PSub subsite

mkYesodSubDataOpts (setParameterizedSubroute True defaultOpts)
  "(PClass subsite master) => PSub subsite" [parseRoutes|
/ PHomeR GET
/item/#Int PItemR GET
!/#{PAssoc subsite}/nested PNestedR:
    / PNestedHomeR GET
    /detail/#Int PNestedDetailR GET
|]

-- | Opt-in kind guard: with parameterized subroutes enabled, the nested
-- subroute datatype is parameterized over the subsite. This signature only
-- compiles if 'PNestedR' has kind @Type -> Type@.
_pNestedRParamGuard :: PNestedR subsite -> PNestedR subsite
_pNestedRParamGuard = id
