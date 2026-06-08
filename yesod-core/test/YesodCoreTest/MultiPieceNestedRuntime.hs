{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | T3: a nested 'ResourceParent' whose OWN path pieces number more than one
-- and include a dynamic capture (@\/shop\/#Int\/admin@). Most nested-route tests
-- use single-piece parents; this stresses the multi-piece machinery —
-- 'handlePiecesNames' binding several pieces, 'mkPathPat' threading them, and
-- the @parentArgsExp@ tuple build that forwards the captured @#Int@ down to the
-- delegated child dispatch — and asserts both the round-trip and the misses.
module YesodCoreTest.MultiPieceNestedRuntime
    ( specs
    ) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Yesod.Core

import YesodCoreTest.RuntimeHarness (assertGet)

data MPApp = MPApp

mkYesod "MPApp" [parseRoutes|
/shop/#Int/admin ShopAdminR:
    /            ShopAdminIndexR  GET
    /users/#Text UserR            GET
|]

instance Yesod MPApp where
    messageLoggerSource = mempty

getShopAdminIndexR :: Int -> HandlerFor MPApp String
getShopAdminIndexR shopId = pure ("admin:" <> show shopId)

getUserR :: Int -> Text -> HandlerFor MPApp String
getUserR shopId name = pure ("user:" <> show shopId <> ":" <> show name)

testRequestIO :: HasCallStack => Int -> [Text] -> Maybe ByteString -> IO ()
testRequestIO = assertGet MPApp

specs :: Spec
specs = describe "multi-piece nested parent (/shop/#Int/admin)" $ do
    it "dispatches the parent index, forwarding the captured #Int" $
        testRequestIO 200 ["shop", "5", "admin"] (Just "admin:5")
    it "dispatches a child, forwarding both the parent #Int and child #Text" $
        testRequestIO 200 ["shop", "5", "admin", "users", "bob"] (Just "user:5:\"bob\"")
    it "404s on an unknown suffix under the multi-piece parent" $
        testRequestIO 404 ["shop", "5", "admin", "nope"] Nothing
    it "404s when the parent's dynamic piece is malformed (#Int given a non-integer)" $
        testRequestIO 404 ["shop", "notanint", "admin"] Nothing
