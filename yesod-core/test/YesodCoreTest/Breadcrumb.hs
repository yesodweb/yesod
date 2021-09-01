{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module YesodCoreTest.Breadcrumb
  ( breadcrumbTest,
  )
where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.Wai
import Network.Wai.Test
import Test.Hspec
import UnliftIO.IORef
import Yesod.Core

data A = A

mkYesod
  "A"
  [parseRoutes|
/    RootR GET
/loop LoopR GET
|]

instance Yesod A

instance YesodBreadcrumbs A where
  breadcrumb r = case r of
    RootR -> pure ("Root", Nothing)
    LoopR -> pure ("Loop", Just LoopR) -- Purposefully a loop

getRootR :: Handler Text
getRootR = fst <$> breadcrumbs

getLoopR :: Handler Text
getLoopR = fst <$> breadcrumbs

breadcrumbTest :: Spec
breadcrumbTest =
  describe "Test.Breadcrumb" $ do
    it "can fetch the root which contains breadcrumbs" $
      runner $ do
        res <- request defaultRequest
        assertStatus 200 res
    it "gets a 500 for a route with a looping breadcrumb" $
      runner $ do
        res <- request defaultRequest {pathInfo = ["loop"]}
        assertStatus 500 res

runner :: Session () -> IO ()
runner f = toWaiApp A >>= runSession f
