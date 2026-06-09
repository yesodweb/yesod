{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The split-out @SubR@ fragment of the parameterized @PApp a@ site. The
-- focused splice ('setFocusOnNestedRoute') here emits only the @SubR a@
-- fragment's dispatch / parse / render / attrs nested instances; the parent
-- "YesodCoreTest.ParamFocusSplit" generates the rest and delegates to these.
-- This module is the compile-time pin for the focused parameterized codegen
-- (the @RouteAttrsNested (SubR a)@ instance head in particular, which used to
-- be built from a bare constructor without the site's type argument).
module YesodCoreTest.ParamFocusSplit.SubR where

import Data.Text (Text)
import Yesod.Core
import YesodCoreTest.ParamFocusSplit.Resources

mkYesodOpts
    (setFocusOnNestedRoute "SubR" (setParameterizedSubroute True defaultOpts))
    "PApp a"
    paramFocusResources

getSubHomeR :: HandlerFor (PApp a) Text
getSubHomeR = pure "subHome"

getSubDetailR :: Int -> HandlerFor (PApp a) Text
getSubDetailR _ = pure "subDetail"
