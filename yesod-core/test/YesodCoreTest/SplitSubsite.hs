{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | End-to-end test that a subsite's nested routes can be split across
-- modules. The nested dispatch + handlers live in
-- "YesodCoreTest.SplitSubsite.NestedR"; here we only define the host site,
-- the subsite's top-level 'YesodSubDispatch' instance (which delegates to the
-- separately compiled 'YesodSubDispatchNested' instance), and the leaf
-- handler. We never import the nested handlers — proving the split.
module YesodCoreTest.SplitSubsite (splitSubsiteSpec) where

import Test.Hspec
import Yesod.Core
import Network.Wai (defaultRequest, pathInfo)
import Network.Wai.Test
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as L8

import YesodCoreTest.SplitSubsite.Data
-- Bring the split-out YesodSubDispatchNested instance into scope. We import
-- the instance only — none of the nested handlers leak into this module.
import YesodCoreTest.SplitSubsite.NestedR ()

data App = App { getSplit :: SplitSub }

mkYesod "App" [parseRoutes|
/split SplitSubR SplitSub getSplit
|]

instance Yesod App

getSplitHomeR :: SubHandlerFor SplitSub master Text
getSplitHomeR = pure "splitHome"

instance YesodSubDispatch SplitSub master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesSplitSub)

app :: App
app = App { getSplit = SplitSub }

runner :: Session () -> IO ()
runner f = toWaiApp app >>= runSession f

splitSubsiteSpec :: Spec
splitSubsiteSpec = describe "YesodCoreTest.SplitSubsite (split subsite routes)" $ do
    it "dispatches the subsite's own leaf route" $ runner $ do
        res <- request defaultRequest { pathInfo = ["split"] }
        assertStatus 200 res
        assertBody (L8.pack "splitHome") res
    it "delegates the nested home route to the split-out module" $ runner $ do
        res <- request defaultRequest { pathInfo = ["split", "nested"] }
        assertStatus 200 res
        assertBody (L8.pack "nestedHome") res
    it "delegates a nested dynamic route to the split-out module" $ runner $ do
        res <- request defaultRequest { pathInfo = ["split", "nested", "detail", "7"] }
        assertStatus 200 res
        assertBody (L8.pack "nestedDetail:7") res
