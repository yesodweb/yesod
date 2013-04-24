{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module YesodCoreTest.Cache (cacheTest, Widget) where

import Test.Hspec

import Network.Wai.Test

import Yesod.Core
import Data.IORef.Lifted
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L8

data C = C

newtype V1 = V1 Int
    deriving Typeable

newtype V2 = V2 Int
    deriving Typeable

mkYesod "C" [parseRoutes|/ RootR GET|]

instance Yesod C

getRootR :: Handler RepPlain
getRootR = do
    ref <- newIORef 0
    V1 v1a <- cached $ atomicModifyIORef ref $ \i -> (i + 1, V1 $ i + 1)
    V1 v1b <- cached $ atomicModifyIORef ref $ \i -> (i + 1, V1 $ i + 1)

    V2 v2a <- cached $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)
    V2 v2b <- cached $ atomicModifyIORef ref $ \i -> (i + 1, V2 $ i + 1)

    return $ RepPlain $ toContent $ show [v1a, v1b, v2a, v2b]

cacheTest :: Spec
cacheTest =
  describe "Test.Cache" $ do
      it "works" works

runner :: Session () -> IO ()
runner f = toWaiApp C >>= runSession f

works :: IO ()
works = runner $ do
    res <- request defaultRequest
    assertStatus 200 res
    assertBody (L8.pack $ show [1, 1, 2, 2 :: Int]) res
