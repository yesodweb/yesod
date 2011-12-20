{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Internal.Cache
    ( Cache
    , CacheKey
    , mkCacheKey
    , lookup
    , insert
    , delete
    ) where

import Prelude hiding (lookup)
import qualified Data.IntMap as Map
import Language.Haskell.TH.Syntax (Q, Exp, runIO, Exp (LitE), Lit (IntegerL))
import Language.Haskell.TH (appE)
import Data.Unique (hashUnique, newUnique)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Monoid (Monoid)
import Control.Applicative ((<$>))

newtype Cache = Cache (Map.IntMap Any)
    deriving Monoid

newtype CacheKey a = CacheKey Int

-- | Generate a new 'CacheKey'. Be sure to give a full type signature.
mkCacheKey :: Q Exp
mkCacheKey = [|CacheKey|] `appE` (LitE . IntegerL . fromIntegral . hashUnique <$> runIO newUnique)

lookup :: CacheKey a -> Cache -> Maybe a
lookup (CacheKey i) (Cache m) = unsafeCoerce <$> Map.lookup i m

insert :: CacheKey a -> a -> Cache -> Cache
insert (CacheKey k) v (Cache m) = Cache (Map.insert k (unsafeCoerce v) m)

delete :: CacheKey a -> Cache -> Cache
delete (CacheKey k) (Cache m) = Cache (Map.delete k m)
