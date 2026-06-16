{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | The same zero-piece-parent shape as "YesodCoreTest.ZeroPieceShadow.ShadowApp",
-- but built with @'setNestedRouteFallthrough' True@. With fallthrough enabled
-- the @WildP@ parent no longer swallows non-matching paths, so the sibling
-- @\/sibling@ becomes reachable. Pinned by "YesodCoreTest.ZeroPieceShadowRuntime".
module YesodCoreTest.ZeroPieceShadow.FallApp
    ( FallApp (..)
    ) where

import Yesod.Core

data FallApp = FallApp

mkYesodOpts (setNestedRouteFallthrough True defaultOpts) "FallApp" [parseRoutes|
/ FallParentR:
    /child FallChildR GET
/sibling FallSiblingR GET
|]

instance Yesod FallApp where
    messageLoggerSource = mempty

getFallChildR :: HandlerFor FallApp String
getFallChildR = pure "FallChildR"

getFallSiblingR :: HandlerFor FallApp String
getFallSiblingR = pure "FallSiblingR"
