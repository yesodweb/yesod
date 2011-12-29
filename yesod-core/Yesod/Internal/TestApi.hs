--
-- | WARNING: This module exposes internal interfaces solely for the
-- purpose of facilitating cabal-driven testing of said interfaces.
-- This module is NOT part of the public Yesod API and should NOT be
-- imported by library users.
--
module Yesod.Internal.TestApi
  ( randomString, parseWaiRequest'
  ) where

import Yesod.Internal.Request (randomString, parseWaiRequest')
