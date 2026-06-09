{-# LANGUAGE TemplateHaskell #-}

-- | Answers the reviewer's question: /can 'isInstance' fail if the route
-- datatype's type parameters remain abstract?/ ('nestedInstanceExists'
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
-- 'isInstance' ever threw (the failure mode the reviewer worried about), the
-- splice would fall through to the 'recover' handler, which 'error's, and the
-- expectation would fail loudly rather than silently.
module Route.InstanceProbeSpec (spec) where

import Test.Hspec
import Language.Haskell.TH (recover)
import Yesod.Routes.TH.Internal (nestedInstanceExists, resolveRouteCon)
import Route.InstanceProbeTypes
    (Probe, HasInst, NoInst, HK, HKInst, Mono)

spec :: Spec
spec = describe "nestedInstanceExists / fullyApplyType abstract-parameter probe" $ do
    it "returns True for an arity-1 datatype that has an instance" $
        $(do
            rc <- resolveRouteCon "HasInst"
            recover [| error "isInstance crashed on HasInst" |] $ do
                b <- nestedInstanceExists ''Probe rc
                [| b |]) `shouldBe` True

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
