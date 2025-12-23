{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module YesodCoreTest.RenderRouteSpec.TH where

import Yesod.Routes.TH.Types
import Language.Haskell.TH
import Yesod.Routes.TH.RenderRoute
import Yesod.Routes.Class
import Test.Hspec
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

