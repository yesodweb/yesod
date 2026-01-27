{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.WaiSubsite
    ( specs
    , Widget
    , resourcesY
    ) where

import YesodCoreTest.YesodTest
import Yesod.Core
import qualified Network.HTTP.Types as H
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B (concat)
import qualified Data.ByteString.Lazy.Char8 as B8 (pack)

myApp :: ByteString -> Application
myApp s _ f = f $ responseLBS H.status200 [("Content-type", "text/plain")] s

getApp :: a -> WaiSubsite
getApp _ = WaiSubsite $ myApp "WAI"

getAppArgs :: a -> Int -> Int -> WaiSubsite
getAppArgs _ i j = WaiSubsite $ myApp $ B.concat ["WAI - ", B8.pack $ show i, " - ", B8.pack $ show j ]

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/sub WaiSubsiteR WaiSubsite getApp
/nested NestedR:
  /sub NestedWaiSubsiteR WaiSubsite getApp
/nestedargs/#Int NestedArgsR:
  /sub/#Int NestedArgsWaiSubsiteR WaiSubsite getAppArgs
|]

instance Yesod Y

app :: Session () -> IO ()
app = yesod Y

getRootR :: Handler ()
getRootR = return ()

specs :: Spec
specs = describe "WaiSubsite" $ do
    it "root" $ app $ do
      res <- request defaultRequest { pathInfo = [] }
      assertStatus 200 res
      assertBodyContains "" res

    it "subsite" $ app $ do
      res <- request defaultRequest { pathInfo = ["sub", "foo"] }
      assertStatus 200 res
      assertBodyContains "WAI" res

    it "nested subsite" $ app $ do
      res <- request defaultRequest { pathInfo = ["nested", "sub", "foo"] }
      assertStatus 200 res
      assertBodyContains "WAI" res

    it "nested subsite with arguments" $ app $ do
      res <- request defaultRequest { pathInfo = ["nestedargs", "1", "sub", "2", "foo"] }
      assertStatus 200 res
      assertBodyContains "WAI - 1 - 2" res
