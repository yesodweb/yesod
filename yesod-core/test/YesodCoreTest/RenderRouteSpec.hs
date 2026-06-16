{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

-- | Behavioural @renderRoute@ spec built from a hand-constructed
-- 'ResourceTree' AST (a @ResourceParent@ with one dynamic piece over a leaf)
-- rather than @parseRoutes@, so it pins @mkRenderRouteInstanceOpts@ directly
-- against a known tree. Companion module "YesodCoreTest.RenderRouteSpec.TH"
-- reuses the generated instance from a compile-time splice.
module YesodCoreTest.RenderRouteSpec where

import Yesod.Routes.TH.Types
import Language.Haskell.TH
import Yesod.Routes.TH.RenderRoute
import Yesod.Routes.Class
import Test.Hspec

data App = App

-- Generate the 'RenderRoute' instance for 'App' at compile time, then check
-- 'renderRoute' behaves at runtime (the prod path; the intermediate clause
-- lists are not asserted directly).
do
    let int = ConT ''Int
    mkRenderRouteInstanceOpts defaultOpts [] NoTyArgs (ConT (mkName "App"))
            [ ResourceParent "FirstR" False mempty [Static "first", Dynamic int]
                [ ResourceLeaf Resource
                    { resourceName = "BlahR"
                    , resourcePieces = [Static "blah"]
                    , resourceDispatch = Methods
                        { methodsMulti = Nothing
                        , methodsMethods = []
                        }
                    , resourceAttrs = []
                    , resourceCheck = False
                    }
                ]
            ]

spec :: Spec
spec = do
    describe "renderRoute" $ do
        it "works on top level" $ do
            case renderRoute (FirstR 1 BlahR) of
                (["first", "1", "blah"], []) ->
                    pure @IO ()
                wrong ->
                    fail $ "Expected renderRoute to work, but got: " <> show wrong
