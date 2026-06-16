{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A pure 'Quote' instance for unit-testing the TH clause builders without
-- 'Q'. The builders are monad-polymorphic over 'Quote' (their only effect is
-- fresh-'Name' generation): production runs them in 'Q' for hygienic 'newName'
-- binders, while these tests run them in 'PureQ', where 'newName' is a
-- deterministic monotonic counter. That makes the generated '[Clause]' AST
-- fully determined and directly comparable — no splicing or 'runQ' needed.
--
-- The deterministic names use non-hygienic 'mkName', which is fine for
-- pure-inspection tests but would risk shadowing in real splices — hence
-- production stays on 'Q' and only the tests pin the builder's monad to 'PureQ'.
module Route.PureQ
    ( PureQ
    , runPureQ
    ) where

import Control.Monad.State.Strict (State, evalState, state)
import Language.Haskell.TH.Syntax (Name, mkName)
import Language.Haskell.TH.Syntax.Compat (Quote(..))

-- | A pure name supply: 'State' over a monotonic counter.
newtype PureQ a = PureQ (State Int a)
    deriving (Functor, Applicative, Monad)

-- | Run a 'PureQ' computation, starting the counter at @0@. Re-running the same
-- builder therefore yields an identical AST (binders and all).
runPureQ :: PureQ a -> a
runPureQ (PureQ s) = evalState s 0

-- | Each 'newName' yields a distinct 'mkName' from the counter, so the builder's
-- output is reproducible and comparable.
instance Quote PureQ where
    newName base = PureQ $ state $ \n -> (mkName (base ++ show n), n + 1)
