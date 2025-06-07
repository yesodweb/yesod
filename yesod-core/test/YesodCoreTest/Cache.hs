{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module YesodCoreTest.Cache
    ( cacheTest
    , Widget
    , resourcesC
    ) where

import Test.Hspec

import Network.Wai
import Network.Wai.Test

import Yesod.Core
import UnliftIO.IORef
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L8

data C = C

newtype V1 = V1 Int

newtype V2 = V2 Int

mkYesod "C" [parseRoutes|
/    RootR GET
/key KeyR GET
/nested NestedR GET
/nested-key NestedKeyR GET
|]

instance Yesod C where
    errorHandler e = liftIO (print e) >> defaultErrorHandler e

getRootR :: Handler RepPlain
getRootR = do
    ref <- newIORef 0
    V1 v1a <- cached $ atomicModifyIORef ref $ \i -> (i + 1, V1 $ i + 1)
    V1 v1b <- cached $ atomicModifyIORef ref $ \i -> (i + 1, V1 $ i + 1)

    V2 v2a <- cached $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)
    V2 v2b <- cached $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)

    cacheBySet "3" (V2 3)
    V2 v3a <- cacheByGet "3" >>= \x ->
      case x of
        Just y -> return y
        Nothing -> error "must be Just"
    V2 v3b <- cachedBy "3" $ (pure $ V2 4)

    return $ RepPlain $ toContent $ show [v1a, v1b, v2a, v2b, v3a, v3b]

getKeyR :: Handler RepPlain
getKeyR = do
    ref <- newIORef 0
    V1 v1a <- cachedBy "1" $ atomicModifyIORef ref $ \i -> (i + 1, V1 $ i + 1)
    V1 v1b <- cachedBy "1" $ atomicModifyIORef ref $ \i -> (i + 1, V1 $ i + 1)

    V2 v2a <- cachedBy "1" $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)
    V2 v2b <- cachedBy "1" $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)

    V2 v3a <- cachedBy "2" $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)
    V2 v3b <- cachedBy "2" $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)


    cacheBySet "4" (V2 4)
    V2 v4a <- cacheByGet "4" >>= \x ->
      case x of
        Just y -> return y
        Nothing -> error "must be Just"
    V2 v4b <- cachedBy "4" $ (pure $ V2 5)

    return $ RepPlain $ toContent $ show [v1a, v1b, v2a, v2b, v3a, v3b, v4a, v4b]

getNestedR :: Handler RepPlain
getNestedR = getNested cached

getNestedKeyR :: Handler RepPlain
getNestedKeyR = getNested $ cachedBy "3"

-- | Issue #1266
getNested ::  (forall a. Typeable a => (Handler a -> Handler a)) -> Handler RepPlain
getNested cacheMethod = do
    ref <- newIORef 0
    let getV2 = atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)
    V1 _ <- cacheMethod $ do
      V2 val <- cacheMethod $ getV2
      return $ V1 val
    V2 v2 <- cacheMethod $ getV2

    return $ RepPlain $ toContent $ show v2

cacheTest :: Spec
cacheTest =
  describe "Test.Cache" $ do
    it "cached" $ runner $ do
      res <- request defaultRequest
      assertStatus 200 res
      assertBody (L8.pack $ show [1, 1, 2, 2, 3, 3 :: Int]) res

    it "cachedBy" $ runner $ do
      res <- request defaultRequest { pathInfo = ["key"] }
      assertStatus 200 res
      assertBody (L8.pack $ show [1, 1, 2, 2, 3, 3, 4, 4 :: Int]) res

    it "nested cached" $ runner $ do
      res <- request defaultRequest { pathInfo = ["nested"] }
      assertStatus 200 res
      assertBody (L8.pack $ show (1 :: Int)) res

    it "nested cachedBy" $ runner $ do
      res <- request defaultRequest { pathInfo = ["nested-key"] }
      assertStatus 200 res
      assertBody (L8.pack $ show (1 :: Int)) res

runner :: Session () -> IO ()
runner f = toWaiApp C >>= runSession f
