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
module YesodCoreTest.ParamSplitSubNested () where

import Yesod.Core
import Yesod.Routes.TH (mkNestedSubDispatchInstance, TyArgs(..))
import Yesod.Routes.Parse (parseType, dropBracket)
import Language.Haskell.TH (Type(..), mkName)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import YesodCoreTest.ParamSubsiteParameterized

getPNestedHomeR :: PAssoc subsite -> SubHandlerFor (PSub subsite) master Text
getPNestedHomeR _ = pure "pNestedHome"

getPNestedDetailR :: PAssoc subsite -> Int -> SubHandlerFor (PSub subsite) master Text
getPNestedDetailR _ _ = pure "pNestedDetail"

-- Generate @YesodSubDispatchNested (PNestedR subsite)@ in this separate module,
-- with the subsite's type argument applied. The context and 'TyArgs' mirror
-- exactly what 'mkYesodSubDispatchInstance' threads internally for the name
-- string @"(PClass subsite master) => PSub subsite"@.
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
        (map (fmap (parseType . dropBracket)) resourcesPSub))
