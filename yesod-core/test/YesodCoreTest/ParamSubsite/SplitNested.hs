{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Split-out dispatch module for the *parameterized* subsite's nested
-- 'PNestedR' fragment. This is the parameterized counterpart to
-- "YesodCoreTest.SplitSubsite.NestedR": the subsite carries a type parameter,
-- so the subroute datatype 'PNestedR' is itself parameterized (kind
-- @Type -> Type@), and the subsite's type argument must be threaded through
-- 'mkNestedSubDispatchInstance' as 'SomeTyArgs'.
--
-- Crucially, the nested handlers ('getPNestedHomeR', 'getPNestedDetailR') live
-- ONLY here, not in the module that defines the parent 'YesodSubDispatch'
-- instance. So that parent splice can compile only by *delegating* to the
-- 'YesodSubDispatchNested' instance generated here (via 'isInstance') — if it
-- tried to inline the nested routes it would reference handlers not in scope.
-- A successful build is therefore proof that the cross-module split works for
-- a parameterized subsite, exercising the @SomeTyArgs@ / @applyTyArgs@ branches
-- of the dispatch codegen that were previously untested.
module YesodCoreTest.ParamSubsite.SplitNested () where

import Yesod.Core
import Yesod.Core.Dispatch
    (mkNestedSubDispatchInstance, TyArgs(..))
import Language.Haskell.TH (Type(..), mkName)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import YesodCoreTest.ParamSubsite.Data

getPNestedHomeR :: PAssoc subsite -> SubHandlerFor (PSub subsite) master Text
getPNestedHomeR _ = pure "pNestedHome"

getPNestedDetailR :: PAssoc subsite -> Int -> SubHandlerFor (PSub subsite) master Text
getPNestedDetailR _ _ = pure "pNestedDetail"

-- Generate @YesodSubDispatchNested (PNestedR subsite)@ in this separate module,
-- with the subsite's type argument applied.
--
-- The 'Cxt' and 'TyArgs' are built manually (rather than parsed from a string
-- like @"(PClass subsite master) => PSub subsite"@) because structured
-- arguments are 'mkNestedSubDispatchInstance''s interface: the string parser
-- belongs to 'mkYesodSubDispatchInstance', the whole-subsite entry point,
-- which does this same construction internally after parsing. A split-out
-- fragment module addresses one nested datatype directly, so it supplies the
-- pieces directly; what is written here is exactly what the parent entry
-- point would thread through for that name string.
$(do
    let subsite = mkName "subsite"
        master  = mkName "master"
        cxt    = [ ConT (mkName "PClass") `AppT` VarT subsite `AppT` VarT master ]
        tyargs = SomeTyArgs ((VarT subsite, subsite) :| [])
    mkNestedSubDispatchInstance
        (setParameterizedSubroute True defaultOpts)
        "PNestedR"
        cxt
        tyargs
        return
        resourcesPSub)
