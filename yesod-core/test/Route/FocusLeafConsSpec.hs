{-# LANGUAGE TemplateHaskell #-}

-- | Regression test for 'mkRouteConsOpts' in focus (module-split) mode.
--
-- Previously the focused branch dropped every 'ResourceLeaf' among the
-- target's children (returning @([], [])@ for leaves), so the focused
-- datatype was generated without its leaf constructors. Here we splice a
-- datatype built directly from 'mkRouteConsOpts'' focused output and then
-- /reference each leaf constructor by name/, so the test fails to compile if
-- any leaf constructor is missing from the generated datatype.
module Route.FocusLeafConsSpec (spec) where

import Test.Hspec
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Yesod.Routes.TH.Types
import Yesod.Routes.TH.RenderRoute
    ( mkRouteConsOpts
    , defaultOpts
    , setFocusOnNestedRoute
    )

data App = App

-- | A nested route @FocusNestR@ whose children are two leaves. We build the
-- focused datatype from 'mkRouteConsOpts' and declare it by hand from the
-- returned constructors.
$(do
    let leaf name pieces =
            ResourceLeaf (Resource name pieces (Methods Nothing ["GET"]) [] True)
        parent name pieces =
            ResourceParent name True mempty pieces
        routes =
            [ leaf "FocusHomeR" []
            , parent "FocusNestR" []
                [ leaf "FocusIndexR" []
                , leaf "FocusShowR" [Dynamic (ConT ''Int)]
                ]
            ]
    (cons, _decs) <-
        mkRouteConsOpts
            (setFocusOnNestedRoute "FocusNestR" defaultOpts)
            []
            NoTyArgs
            (ConT ''App)
            routes
    -- Also bind @focusNestConNames :: [String]@ to the names of exactly the
    -- constructors 'mkRouteConsOpts' returned, so the spec can assert the
    -- complete constructor set — nothing missing, and nothing extra. (Built in
    -- the same splice because a later splice cannot 'reify' a datatype this
    -- one declares — the same-module limitation the probe/arity Types modules
    -- exist to work around.)
    let conName con = case con of
            NormalC n _ -> pure (nameBase n)
            RecC n _ -> pure (nameBase n)
            _ -> fail $ "FocusLeafConsSpec: unexpected constructor shape: " <> show con
    namesE <- lift =<< traverse conName cons
    let namesName = mkName "focusNestConNames"
    pure
        [ DataD [] (mkName "FocusNestR") [] Nothing cons [DerivClause Nothing [ConT ''Show]]
        , SigD namesName (AppT ListT (ConT ''String))
        , ValD (VarP namesName) (NormalB namesE) []
        ])

spec :: Spec
spec = describe "mkRouteConsOpts (focus mode)" $ do
    it "keeps the focused route's leaf constructors" $ do
        -- These references only typecheck if the constructors exist on the
        -- generated datatype; the leaf-drop bug omitted them entirely.
        show FocusIndexR `shouldBe` "FocusIndexR"
        show (FocusShowR 7) `shouldBe` "FocusShowR 7"
    it "generates exactly the two leaf constructors and nothing else" $
        focusNestConNames `shouldBe` ["FocusIndexR", "FocusShowR"]
