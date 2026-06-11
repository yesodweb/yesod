{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | A zero-piece nested parent (pattern @\/@) followed by a sibling, built with
-- 'defaultOpts'. The parent's match pattern is unconditional,
-- so @\/sibling@ is shadowed: it is swallowed by the parent and 404s.
-- Pinned by "YesodCoreTest.ZeroPieceShadowRuntime".
module YesodCoreTest.ZeroPieceShadow.ShadowApp
    ( ShadowApp (..)
    ) where

import Yesod.Core

data ShadowApp = ShadowApp

mkYesodOpts defaultOpts "ShadowApp" [parseRoutes|
/ ShadowParentR:
    /child ShadowChildR GET
/sibling SiblingR GET
|]

instance Yesod ShadowApp where
    messageLoggerSource = mempty

getShadowChildR :: HandlerFor ShadowApp String
getShadowChildR = pure "ShadowChildR"

getSiblingR :: HandlerFor ShadowApp String
getSiblingR = pure "SiblingR"
