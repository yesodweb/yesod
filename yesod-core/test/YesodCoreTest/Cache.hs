{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.Cache (cacheTest, Widget) where

import Test.Hspec
import Test.Hspec.HUnit()

import Network.Wai
import Network.Wai.Test

import Yesod.Core

data C = C

key :: CacheKey Int
key = $(mkCacheKey)

key2 :: CacheKey Int
key2 = $(mkCacheKey)

mkYesod "C" [parseRoutes|/ RootR GET|]

instance Yesod C

getRootR :: Handler ()
getRootR = do
    Nothing <- cacheLookup key
    cacheInsert key 5
    Just 5 <- cacheLookup key
    cacheInsert key 7
    Just 7 <- cacheLookup key
    Nothing <- cacheLookup key2
    cacheDelete key
    Nothing <- cacheLookup key
    return ()

cacheTest :: [Spec]
cacheTest =
  describe "Test.Cache"
    [ it "works" works
    ]

runner :: Session () -> IO ()
runner f = toWaiApp C >>= runSession f

works :: IO ()
works = runner $ do
    res <- request defaultRequest { pathInfo = [] }
    assertStatus 200 res
