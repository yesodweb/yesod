{-# language TemplateHaskell #-}

-- | Mildly cursed module. Write test assertions in Template Haskell.
module YesodCoreTest.Assertions.TH where

import Language.Haskell.TH

shouldBeTH :: (Eq a, Show a) => a -> a -> Q ()
shouldBeTH lhs rhs =
    if a == b
        then pure ()
        else fail $ "Expected " <> show rhs <> ", but got " <> show lhs

