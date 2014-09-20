{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module YesodCoreTest.Cache (cacheTest, Widget) where

import Test.Hspec

import Network.Wai
import Network.Wai.Test

import Yesod.Core
import Data.IORef.Lifted
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

data C = C

newtype V1 = V1 Int
    deriving Typeable

newtype V2 = V2 Int
    deriving Typeable

mkYesod "C" [parseRoutes|
/    RootR GET
/key KeyR GET
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

    return $ RepPlain $ toContent $ show [v1a, v1b, v2a, v2b]

getKeyR :: Handler RepPlain
getKeyR = do
    ref <- newIORef 0
    V1 v1a <- cachedBy "1" $ atomicModifyIORef ref $ \i -> (i + 1, V1 $ i + 1)
    V1 v1b <- cachedBy "1" $ atomicModifyIORef ref $ \i -> (i + 1, V1 $ i + 1)

    V2 v2a <- cachedBy "1" $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)
    V2 v2b <- cachedBy "1" $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)

    V2 v3a <- cachedBy "2" $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)
    V2 v3b <- cachedBy "2" $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)

    return $ RepPlain $ toContent $ show [v1a, v1b, v2a, v2b, v3a, v3b]

cacheTest :: Spec
cacheTest =
  describe "Test.Cache" $ do
    it "cached" $ runner $ do
      res <- request defaultRequest
      assertStatus 200 res
      assertBody (L8.pack $ show [1, 1, 2, 2 :: Int]) res

    it "cachedBy" $ runner $ do
      res <- request defaultRequest { pathInfo = ["key"] }
      assertStatus 200 res
      assertBody (L8.pack $ show [1, 1, 2, 2, 3, 3 :: Int]) res

runner :: Session () -> IO ()
runner f = toWaiApp C >>= runSession f
