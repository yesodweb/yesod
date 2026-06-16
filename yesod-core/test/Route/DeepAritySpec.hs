{-# LANGUAGE TemplateHaskell #-}

-- | Exercises the reify/'typeArity' path that 'assertNestedSubArity' runs
-- inside 'mkNestedDispatchInstanceWith'\'s recursion — the part the pure
-- 'checkNestedSubArity' unit tests in "Route.SubDispatchAritySpec" can't reach.
-- A 2nd-level-or-deeper nested datatype whose arity doesn't match the site's
-- type-argument count must be caught by the guard's @fail@ (an actionable
-- 'ArityMismatch') rather than slip through to a cryptic kind error in
-- generated code.
module Route.DeepAritySpec (spec) where

import Test.Hspec
import Language.Haskell.TH (recover)
import Yesod.Routes.TH.Internal
    ( assertNestedSubArity
    , resolveRouteCon
    , typeArity
    , ArityCallSite(..)
    , SubsiteName(..)
    , SubsiteArity(..)
    )
import Route.DeepArityTypes (ParamSite, MatchingDeepR, WrongDeepR)

-- | The reified arities feeding the guard, extracted at compile time through
-- the same 'typeArity' call 'assertNestedSubArity' makes.
reifiedArities :: (Int, Int, Int)
reifiedArities =
    $(do
        a <- typeArity ''ParamSite
        b <- typeArity ''MatchingDeepR
        c <- typeArity ''WrongDeepR
        [| (a, b, c) |])

-- | 'True' iff the arity guard fired its @fail@ for a (site arity 1) vs.
-- (@WrongDeepR@ arity 0) pairing — i.e. the deep mismatch was caught at the
-- guard rather than escaping into ill-kinded generated code.
wrongArityCaught :: Bool
wrongArityCaught =
    $(do
        rc <- resolveRouteCon "WrongDeepR"
        recover [| True |] $ do
            assertNestedSubArity TopLevelCall (SubsiteName "ParamSite") (SubsiteArity 1) rc
            [| False |])

-- | 'True' iff a matching (arity 1 vs. 1) pairing passes the guard cleanly.
matchingArityOk :: Bool
matchingArityOk =
    $(do
        rc <- resolveRouteCon "MatchingDeepR"
        recover [| False |] $ do
            assertNestedSubArity TopLevelCall (SubsiteName "ParamSite") (SubsiteArity 1) rc
            [| True |])

spec :: Spec
spec = describe "deep nested-route arity guard (reify path)" $ do
    it "reifies datatype arities used by the guard" $
        reifiedArities `shouldBe` (1, 1, 0)
    it "catches a deep arity mismatch with the actionable guard (not a kind error)" $
        wrongArityCaught `shouldBe` True
    it "lets a correctly-parameterized deep nested datatype through" $
        matchingArityOk `shouldBe` True
