{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module YesodCoreTest.RenderRouteSpec where

import Yesod.Routes.TH.Types
import Language.Haskell.TH
import Yesod.Routes.TH.RenderRoute
import Yesod.Routes.Class
import Test.Hspec

-- This test is mostly done at compile-time.
do
    let int = ConT ''Int
    (clauses, names) <- mkRenderRouteClauses
        [ ResourceParent "FirstR" False [Static "first", Dynamic int]
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
    case clauses of
        [Clause [pat] (NormalB expr) []] ->
            case expr of
                VarE ((== 'renderRouteNested) -> True) `AppE` (VarE x) `AppE` (VarE c) ->
                    pure ()
                _ ->
                    fail $ "Expr wrong shape: " <> show expr
        _ ->
            fail $ "Got the wrong clauses: " <> show clauses

    pure []

do
    let int = ConT ''Int
        parentName = mkName "parent"
    (clauses, names) <- mkRenderRouteNestedClauses
        [Left "hello", Right parentName]
        [ ResourceParent "FirstR" False [Static "first", Dynamic int]
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

    case clauses of
        [Clause [parentArgPat, routePat] (NormalB expr) []] -> do
            case parentArgPat of
                VarP _ ->
                    pure ()
                _ ->
                    fail $ "Wrong parentArgPat: " <> show parentArgPat

            case routePat of
                ConP ((mkName "FirstR" ==) -> True) [] [VarP n, VarP c] ->
                    pure ()
                _ ->
                    fail $ "Wrong routePat: " <> show routePat

            case expr of
                VarE ((== 'renderRouteNested) -> True) `AppE` (VarE x) ->
                    pure ()
                _ ->
                    fail $ "Expr wrong shape: " <> show expr
        _ ->
            fail $ "Got the wrong clauses: " <> show clauses

    pure []

do
    let int = ConT ''Int
        parentName = mkName "parent"
    (clauses, names) <- mkRenderRouteNestedClauses
        [Left "hello", Right parentName]
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

    case clauses of
        [Clause [parentArgPat, routePat] (NormalB expr) []] -> do
            case parentArgPat of
                VarP _ ->
                    pure ()
                _ ->
                    fail $ "Wrong parentArgPat: " <> show parentArgPat

            case routePat of
                ConP ((mkName "BlahR" ==) -> True) [] [] ->
                    pure ()
                _ ->
                    fail $ "Wrong routePat: " <> show routePat

        _ ->
            fail $ "Got the wrong clauses: " <> show clauses

    pure []

data App = App

do
    let int = ConT ''Int
    mkRenderRouteInstanceOpts defaultOpts [] [] (ConT (mkName "App"))
            [ ResourceParent "FirstR" False [Static "first", Dynamic int]
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
                    fail $ "Expecte renderRoute to work, but got: " <> show wrong
