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
import Yesod.Routes.TH.Types
import Yesod.Routes.TH.RenderRoute
    ( mkRouteConsOpts
    , defaultOpts
    , setFocusOnNestedRoute
    )

data App = App

-- | A nested route @FocusNestR@ whose children are two leaves and one inner
-- parent. We build the focused datatype from 'mkRouteConsOpts' and declare it
-- by hand from the returned constructors.
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
    pure [DataD [] (mkName "FocusNestR") [] Nothing cons [DerivClause Nothing [ConT ''Show]]])

spec :: Spec
spec = describe "mkRouteConsOpts (focus mode)" $
    it "keeps the focused route's leaf constructors" $ do
        -- These references only typecheck if the constructors exist on the
        -- generated datatype; the leaf-drop bug omitted them entirely.
        show FocusIndexR `shouldBe` "FocusIndexR"
        show (FocusShowR 7) `shouldBe` "FocusShowR 7"
