-- | Datatypes for "Route.DeepAritySpec", in their own module so the spec's
-- compile-time splices can 'reify' them — a datatype defined in the same module
-- as a top-level splice is not yet in the type environment at a 'reify'.
module Route.DeepArityTypes
    ( ParamSite (..)
    , MatchingDeepR (..)
    , WrongDeepR (..)
    ) where

-- A parameterized site (arity 1) plus two candidate nested datatypes: one whose
-- arity matches (1) and one whose arity is wrong (0). These stand in for a
-- deeply-nested subroute reached by the dispatch recursion.
data ParamSite a = ParamSite
data MatchingDeepR a = MatchingDeepR
data WrongDeepR = WrongDeepR
