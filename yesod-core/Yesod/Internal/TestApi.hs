--
-- | WARNING: This module exposes internal interfaces solely for the
-- purpose of facilitating unit testing with cabal install. Library
-- users should not import this module.
--
module Yesod.Internal.TestApi
  ( Request (..), randomString, parseWaiRequest'
  ) where

import Yesod.Internal.Request (Request (..), randomString, parseWaiRequest')
