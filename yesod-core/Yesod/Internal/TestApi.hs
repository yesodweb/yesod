--
-- | WARNING: This module exposes internal interfaces solely for the
-- purpose of facilitating cabal-driven testing of said interfaces.
-- This module is NOT part of the public Yesod API and should NOT be
-- imported by library users.
--
module Yesod.Internal.TestApi
  ( randomString, parseWaiRequest'
  , catchIter
  ) where

import Yesod.Internal.Request (randomString, parseWaiRequest')
import Control.Exception (Exception, catch)
import Data.Enumerator (Iteratee (..))
import Data.ByteString (ByteString)
import Prelude hiding (catch)

catchIter :: Exception e
          => Iteratee ByteString IO a
          -> (e -> Iteratee ByteString IO a)
          -> Iteratee ByteString IO a
catchIter (Iteratee mstep) f = Iteratee $ mstep `catch` (runIteratee . f)
