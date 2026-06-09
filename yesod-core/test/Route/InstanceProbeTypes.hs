{-# LANGUAGE KindSignatures #-}

-- | Datatypes and a stand-in probe class for "Route.InstanceProbeSpec", in
-- their own module so the spec's compile-time splices can 'reify' them — a
-- datatype defined in the same module as a top-level splice is not yet in the
-- type environment at a 'reify' (the same reason "Route.DeepArityTypes" is
-- separate).
--
-- 'Probe' stands in for any of the real nested-discovery classes
-- (@YesodDispatchNested@, @RenderRouteNested@, …) that 'nestedInstanceExists'
-- probes: those classes aren't visible to the pure @test-routes@ suite, but
-- 'nestedInstanceExists' only ever asks @isInstance klass [head]@, so any
-- single-parameter class with the right instance shape exercises the same
-- code path.
module Route.InstanceProbeTypes
    ( Probe
    , HasInst (..)
    , NoInst (..)
    , HK (..)
    , HKInst (..)
    , Mono (..)
    ) where

import Data.Kind (Type)

-- | A single-parameter probe class, standing in for the nested-discovery
-- classes 'nestedInstanceExists' checks against.
class Probe a

-- | Arity-1, kind-'Type' parameter, /with/ a @Probe@ instance — the probe must
-- answer 'True'. The instance is given at a fully abstract parameter, exactly
-- as our codegen emits per-route-datatype instances.
data HasInst a = HasInst
instance Probe (HasInst a)

-- | Arity-1, kind-'Type' parameter, /without/ a @Probe@ instance — the probe
-- must answer 'False'.
data NoInst a = NoInst

-- | Arity-1 with a kind-annotated, non-'Type' parameter @(f :: Type -> Type)@
-- and /no/ instance. The point of this case: 'fullyApplyType' saturates @HK@
-- with a bare, unannotated @VarT@ whose kind GHC must infer from @HK@'s
-- declared kind. The probe must not crash and must report 'False'.
data HK (f :: Type -> Type) = HK

-- | As 'HK', but /with/ an instance given at an abstract higher-kinded
-- parameter. The probe must report 'True' without crashing.
data HKInst (f :: Type -> Type) = HKInst
instance Probe (HKInst f)

-- | Arity-0 (the zero type-parameter case): 'fullyApplyType' applies no
-- arguments, so the probed head is the bare constructor. With an instance the
-- probe answers 'True'.
data Mono = Mono
instance Probe Mono
