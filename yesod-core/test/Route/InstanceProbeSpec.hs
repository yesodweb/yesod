{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Pins the answer to: /can 'isInstance' fail if the route datatype's
-- type parameters remain abstract?/ ('nestedInstanceExists'
-- saturates a resolved datatype with fresh, unannotated 'VarT's via
-- 'fullyApplyType' rather than ground types.)
--
-- The exercised contract:
--
--   * Saturation to the datatype's /own/ reified arity keeps the probed head
--     well-kinded, so the probe is a total query — \"could any instance
--     match\" — that returns 'True'\/'False' but never aborts the splice.
--   * It holds even when a parameter is higher-kinded (@(f :: Type -> Type)@):
--     the bare @VarT@ 'fullyApplyType' supplies has its kind inferred from the
--     datatype's declared kind, so the head is still well-kinded.
--   * The arity-0 case (no parameters) degenerates to probing the bare
--     constructor.
--
-- Each probe runs inside a compile-time splice wrapped in 'recover': if
-- 'isInstance' ever threw (the failure mode guarded against here), the
-- splice would fall through to the 'recover' handler, which 'error's, and the
-- expectation would fail loudly rather than silently.
module Route.InstanceProbeSpec (spec) where

import Test.Hspec
import Language.Haskell.TH (recover, isInstance, Type (ConT, AppT))
import Yesod.Routes.TH.Internal (nestedInstanceExists, resolveRouteCon)
import Route.InstanceProbeTypes
    ( Probe, HasInst, HasInst2, HasInstInt, NoInst, HK, HKInst, Mono
    , ProbePoly, PolyFull, PolyUnapplied, PolyPartial
    )

spec :: Spec
spec = describe "nestedInstanceExists / fullyApplyType abstract-parameter probe" $ do
    it "returns True for an arity-1 datatype that has an instance" $
        $(do
            rc <- resolveRouteCon "HasInst"
            recover [| error "isInstance crashed on HasInst" |] $ do
                b <- nestedInstanceExists ''Probe rc
                [| b |]) `shouldBe` True

    it "returns True for an arity-2 datatype with an instance at abstract parameters" $
        $(do
            rc <- resolveRouteCon "HasInst2"
            recover [| error "isInstance crashed on HasInst2" |] $ do
                b <- nestedInstanceExists ''Probe rc
                [| b |]) `shouldBe` True

    -- Whether probing the abstract head @HasInstInt a@ finds the concrete
    -- @instance Probe (HasInstInt Int)@ depends on the GHC version: GHC 9.0+
    -- 'reifyInstances' returns unifiers (so a bare type variable in the query
    -- unifies with @Int@ and the probe reports 'True'), whereas GHC < 9.0 does
    -- not return the concrete-argument instance for a type-variable query head
    -- (so it reports 'False'). This concrete-argument shape never arises in real
    -- codegen — nested-discovery instances are always emitted at fully-abstract
    -- parameters (the 'HasInst' case above), which every supported GHC resolves
    -- identically — so this case only pins the observed 'reifyInstances'
    -- divergence, not a behaviour the codegen relies on.
#if __GLASGOW_HASKELL__ >= 900
    it "returns True when the only instance is at a concrete argument (a unifier counts as could-match)" $
        $(do
            rc <- resolveRouteCon "HasInstInt"
            recover [| error "isInstance crashed on HasInstInt" |] $ do
                b <- nestedInstanceExists ''Probe rc
                [| b |]) `shouldBe` True
#else
    it "returns False when the only instance is at a concrete argument (GHC < 9.0 reifyInstances does not unify a type-variable query head)" $
        $(do
            rc <- resolveRouteCon "HasInstInt"
            recover [| error "isInstance crashed on HasInstInt" |] $ do
                b <- nestedInstanceExists ''Probe rc
                [| b |]) `shouldBe` False
#endif

    it "returns False for an arity-1 datatype with no instance" $
        $(do
            rc <- resolveRouteCon "NoInst"
            recover [| error "isInstance crashed on NoInst" |] $ do
                b <- nestedInstanceExists ''Probe rc
                [| b |]) `shouldBe` False

    it "does not crash on a higher-kinded (f :: Type -> Type) parameter with no instance (returns False)" $
        $(do
            rc <- resolveRouteCon "HK"
            recover [| error "isInstance crashed on HK" |] $ do
                b <- nestedInstanceExists ''Probe rc
                [| b |]) `shouldBe` False

    it "does not crash on a higher-kinded parameter with an instance (returns True)" $
        $(do
            rc <- resolveRouteCon "HKInst"
            recover [| error "isInstance crashed on HKInst" |] $ do
                b <- nestedInstanceExists ''Probe rc
                [| b |]) `shouldBe` True

    it "handles the zero-arity case (bare constructor head) with an instance" $
        $(do
            rc <- resolveRouteCon "Mono"
            recover [| error "isInstance crashed on Mono" |] $ do
                b <- nestedInstanceExists ''Probe rc
                [| b |]) `shouldBe` True

    -- Poly-kinded class: lets us represent (and so probe) instances whose head
    -- is not fully applied — the shapes a 'Type'-kinded class rejects outright.
    it "ProbePoly: returns True for an instance at the fully-applied head" $
        $(do
            rc <- resolveRouteCon "PolyFull"
            recover [| error "isInstance crashed on PolyFull" |] $ do
                b <- nestedInstanceExists ''ProbePoly rc
                [| b |]) `shouldBe` True

    it "ProbePoly: returns False when the only instance is at the unapplied constructor (the arity-saturated probe head doesn't match it)" $
        $(do
            rc <- resolveRouteCon "PolyUnapplied"
            recover [| error "isInstance crashed on PolyUnapplied" |] $ do
                b <- nestedInstanceExists ''ProbePoly rc
                [| b |]) `shouldBe` False

    it "ProbePoly: returns False when the only instance is at a partially-applied head" $
        $(do
            rc <- resolveRouteCon "PolyPartial"
            recover [| error "isInstance crashed on PolyPartial" |] $ do
                b <- nestedInstanceExists ''ProbePoly rc
                [| b |]) `shouldBe` False

    -- Why 'nestedInstanceExists' must saturate to the datatype's *full* arity:
    -- the partial instance @ProbePoly (PolyPartial a)@ matches *any* one-argument
    -- application of @PolyPartial@. Probing one argument short — at @PolyPartial
    -- Int@ (kind Type -> Type) — therefore matches it, while the full-arity head
    -- @PolyPartial a b@ (kind Type, what the probe actually uses) does not. So a
    -- probe that stopped short would be spuriously fooled by a partial instance;
    -- saturating fully is what avoids that. These pin that matching boundary by
    -- calling 'isInstance' directly at hand-built heads.
    it "isInstance matches the partial instance one argument short (PolyPartial Int)" $
        $(do
            recover [| error "isInstance crashed on PolyPartial Int" |] $ do
                b <- isInstance ''ProbePoly [ConT ''PolyPartial `AppT` ConT ''Int]
                [| b |]) `shouldBe` True

    it "isInstance does not match the partial instance at the full application (PolyPartial Int Int)" $
        $(do
            recover [| error "isInstance crashed on PolyPartial Int Int" |] $ do
                b <- isInstance ''ProbePoly [ConT ''PolyPartial `AppT` ConT ''Int `AppT` ConT ''Int]
                [| b |]) `shouldBe` False

    it "isInstance does not match the unapplied-constructor instance once an argument is applied (PolyUnapplied Int)" $
        $(do
            recover [| error "isInstance crashed on PolyUnapplied Int" |] $ do
                b <- isInstance ''ProbePoly [ConT ''PolyUnapplied `AppT` ConT ''Int]
                [| b |]) `shouldBe` False
