{-# LANGUAGE TemplateHaskell #-}
-- | This module pins the constructor set of the generated @FocusNestR@
-- datatype exactly, so escalate an incomplete 'describeFocus' to an error.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | Regression test for 'mkRouteConsOpts' in focus (module-split) mode.
--
-- Previously the focused branch dropped every 'ResourceLeaf' among the
-- target's children (returning @([], [])@ for leaves), so the focused
-- datatype was generated without its leaf constructors. Here we splice a
-- datatype built directly from 'mkRouteConsOpts'' focused output, then assert
-- its constructor set is /exactly/ what we expect via a total 'case':
--
--   * a /missing/ constructor fails the pattern reference (a name that isn't
--     in scope), and
--   * an /extra/ constructor makes 'describeFocus' non-exhaustive, which
--     @-Werror=incomplete-patterns@ (set above) turns into a compile error.
--
-- So this module fails to build unless @FocusNestR@ has precisely the two leaf
-- constructors @FocusIndexR@ and @FocusShowR Int@.
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
    pure [DataD [] (mkName "FocusNestR") [] Nothing cons [DerivClause Nothing [ConT ''Show]]])

-- | Total over every constructor of the generated @FocusNestR@. Naming each
-- constructor exactly once is what makes this a complete-set assertion: see
-- the module header for how missing/extra constructors are each rejected at
-- compile time.
describeFocus :: FocusNestR -> String
describeFocus r = case r of
    FocusIndexR  -> "FocusIndexR"
    FocusShowR n -> "FocusShowR " ++ show n

spec :: Spec
spec = describe "mkRouteConsOpts (focus mode)" $
    it "generates exactly the focused route's leaf constructors" $ do
        -- The real assertion is at compile time (see 'describeFocus'); these
        -- just exercise the generated constructors.
        describeFocus FocusIndexR    `shouldBe` "FocusIndexR"
        describeFocus (FocusShowR 7) `shouldBe` "FocusShowR 7"
