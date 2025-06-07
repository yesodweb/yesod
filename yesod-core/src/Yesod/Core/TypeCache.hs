-- | a module for caching a monadic action based on its return type
--
-- The cache is a HashMap where the key uses the TypeReP from Typeable.
-- The value stored is toDyn from Dynamic to support arbitrary value types in the same Map.
--
-- un-exported newtype wrappers should be used to maintain unique keys in the cache.
-- Note that a TypeRep is unique to a module in a package, so types from different modules will not conflict if they have the same name.
--
-- used in 'Yesod.Core.Handler.cached' and 'Yesod.Core.Handler.cachedBy'
module Yesod.Core.TypeCache (cached, cacheGet, cacheSet, cachedBy, cacheByGet, cacheBySet, TypeMap, KeyedTypeMap) where

import           Prelude hiding (lookup)
import           Data.Typeable                      (Typeable, TypeRep, typeOf)
import           Data.HashMap.Strict
import           Data.ByteString                    (ByteString)
import           Data.Dynamic                       (Dynamic, toDyn, fromDynamic)

type TypeMap      = HashMap TypeRep Dynamic
type KeyedTypeMap = HashMap (TypeRep, ByteString) Dynamic

-- | avoid performing the same action multiple times.
-- Values are stored by their TypeRep from Typeable.
-- Therefore, you should use un-exported newtype wrappers for each cache.
--
-- For example, yesod-auth uses an un-exported newtype, CachedMaybeAuth and exports functions that utilize it such as maybeAuth.
-- This means that another module can create its own newtype wrapper to cache the same type from a different action without any cache conflicts.
--
-- In Yesod, this is used for a request-local cache that is cleared at the end of every request.
-- See the original announcement: <http://www.yesodweb.com/blog/2013/03/yesod-1-2-cleaner-internals>
--
-- Since 1.4.0
cached :: (Monad m, Typeable a)
       => TypeMap
       -> m a                       -- ^ cache the result of this action
       -> m (Either (TypeMap, a) a) -- ^ Left is a cache miss, Right is a hit
cached cache action = case cacheGet cache of
    Just val -> return $ Right val
    Nothing -> do
        val <- action
        return $ Left (cacheSet val cache, val)

-- | Retrieves a value from the cache
--
-- @since 1.6.10
cacheGet :: Typeable a => TypeMap -> Maybe a
cacheGet cache = res
  where
    res = lookup (typeOf $ fromJust res) cache >>= fromDynamic
    fromJust :: Maybe a -> a
    fromJust = error "Yesod.Handler.cached.fromJust: Argument to typeOf was evaluated"

-- | Sets a value in the cache
--
-- @since 1.6.10
cacheSet :: (Typeable a)
         => a
         -> TypeMap
         -> TypeMap
cacheSet v cache = insert (typeOf v) (toDyn v) cache

-- | similar to 'cached'.
-- 'cached' can only cache a single value per type.
-- 'cachedBy' stores multiple values per type by indexing on a ByteString key
--
-- 'cached' is ideal to cache an action that has only one value of a type, such as the session's current user
-- 'cachedBy' is required if the action has parameters and can return multiple values per type.
-- You can turn those parameters into a ByteString cache key.
-- For example, caching a lookup of a Link by a token where multiple token lookups might be performed.
--
-- Since 1.4.0
cachedBy :: (Monad m, Typeable a)
         => KeyedTypeMap
         -> ByteString                     -- ^ a cache key
         -> m a                            -- ^ cache the result of this action
         -> m (Either (KeyedTypeMap, a) a) -- ^ Left is a cache miss, Right is a hit
cachedBy cache k action = case cacheByGet k cache of
    Just val -> return $ Right val
    Nothing -> do
        val <- action
        return $ Left (cacheBySet k val cache, val)

-- | Retrieves a value from the keyed cache
--
-- @since 1.6.10
cacheByGet :: Typeable a => ByteString -> KeyedTypeMap -> Maybe a
cacheByGet key c = res
  where
    res = lookup (typeOf $ fromJust res, key) c >>= fromDynamic
    fromJust :: Maybe a -> a
    fromJust = error "Yesod.Handler.cached.fromJust: Argument to typeOf was evaluated"

-- | Sets a value in the keyed cache
--
-- @since 1.6.10
cacheBySet :: Typeable a => ByteString -> a -> KeyedTypeMap -> KeyedTypeMap
cacheBySet key v cache = insert (typeOf v, key) (toDyn v) cache
