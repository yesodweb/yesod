{-# LANGUAGE LambdaCase #-}

-- | module providing an abstract type around 'CatchBehavior'
--   through smart constructors.
--   providing future additional extensibility.
--
--   @since 1.6.24.0
module Yesod.Core.CatchBehavior(CatchBehavior, rethrow, catch, isCatch) where

-- | @since 1.6.24.0
data CatchBehavior = Rethrow -- ^ Rethrow an exception and let the webserver deal with it (usually warp)
                   | Catch -- ^ catch an exception and render in yesod

rethrow :: CatchBehavior
rethrow = Rethrow

catch :: CatchBehavior
catch = Catch

isCatch :: CatchBehavior -> Bool
isCatch = \case
  Catch   -> True
  Rethrow -> False
