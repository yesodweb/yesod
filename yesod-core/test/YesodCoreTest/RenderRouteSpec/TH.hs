{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

-- | Compile-time companion to "YesodCoreTest.RenderRouteSpec": reuses that
-- module's generated @RenderRoute App@ instance and @fail@s the splice unless
-- both @renderRoute@ (full route) and @renderRouteNested@ (from the leaf, given
-- the parent's captured arg) reconstruct the same path. Lives apart from the
-- generating module so the instance is in scope when these top-level splices run.
module YesodCoreTest.RenderRouteSpec.TH where

import Yesod.Routes.Class
import YesodCoreTest.RenderRouteSpec

do
    case renderRoute (FirstR 1 BlahR) of
        (["first", "1", "blah"], []) ->
            pure ()
        wrong ->
            fail $ "Expecte renderRoute to work, but got: " <> show wrong

    pure []

do
    case renderRouteNested 1 BlahR of
        (["first", "1", "blah"], []) ->
            pure []
        wrong ->
            fail $ "Expected renderRouteNested to work, but got: " <> show wrong

