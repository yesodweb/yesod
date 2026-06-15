{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

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
    , HasInst2 (..)
    , HasInstInt (..)
    , NoInst (..)
    , HK (..)
    , HKInst (..)
    , Mono (..)
      -- * Poly-kinded probe (under-applied instance heads)
    , ProbePoly
    , PolyFull (..)
    , PolyUnapplied (..)
    , PolyPartial (..)
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

-- | Arity-2: 'fullyApplyType' must saturate with /two/ fresh 'VarT's before
-- the head is well-kinded. With an instance at fully abstract parameters the
-- probe answers 'True'.
data HasInst2 a b = HasInst2
instance Probe (HasInst2 a b)

-- | Arity-1 whose only instance is at a /concrete/ argument. The probe asks
-- \"could any instance match\" ('reifyInstances' returns unifiers, not just
-- exact matches), so probing the abstract @HasInstInt a@ finds the @Int@
-- instance and answers 'True'.
--
-- (Instances at an /under-applied/ constructor — the bare @instance Probe
-- HasInstInt@, or an arity-2 datatype applied to one argument like
-- @instance Probe (HasInst2 a)@ — can't be written against this 'Type'-kinded
-- 'Probe': a head of kind @Type -> Type@ is kind-rejected at its definition
-- site (GHC: \"Expecting one more argument to …; Expected a type, but … has
-- kind @* -> *@\"). To probe those shapes at all they must be made
-- representable by a /poly-kinded/ class; see 'ProbePoly' below.)
data HasInstInt a = HasInstInt
instance Probe (HasInstInt Int)

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

-- | A /poly-kinded/ probe class. Unlike 'Probe' (kind @Type -> Constraint@),
-- @ProbePoly@ accepts a head of any kind, which makes the under-applied
-- instance shapes that 'Probe' rejects at their definition site representable —
-- so we can pin down what 'nestedInstanceExists' does when the only instance is
-- at such a head. The probe always saturates the datatype to its /own/ arity
-- (a kind-'Type' head; see 'fullyApplyType'), so these cases also confirm it
-- queries at the fully-applied head specifically.
class ProbePoly (a :: k)

-- | Poly-kinded class, instance at the /fully-applied/ head (kind 'Type').
-- Positive control: the probe saturates @PolyFull@ to @PolyFull a@ and matches.
data PolyFull a = PolyFull
instance ProbePoly (PolyFull a)

-- | Instance at the /unapplied/ arity-1 constructor (head kind @Type -> Type@),
-- representable only because 'ProbePoly' is poly-kinded. The probe saturates to
-- @PolyUnapplied a@ before querying, so the instance at the bare constructor is
-- a different head.
data PolyUnapplied a = PolyUnapplied
instance ProbePoly PolyUnapplied

-- | Instance at a /partially-applied/ arity-2 constructor (head kind
-- @Type -> Type@). As 'PolyUnapplied', the full-arity probe head
-- @PolyPartial a b@ is a different head from the partial instance @PolyPartial a@.
data PolyPartial a b = PolyPartial
instance ProbePoly (PolyPartial a)
