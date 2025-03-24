{-# LANGUAGE OverloadedStrings #-}
module YesodCoreTest.Content (specs) where

import Yesod.Core
import Yesod.Core.Content
import Test.Hspec
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

fakeContent :: String -> String -> TypedContent
fakeContent contentTypeString contentString =
  TypedContent
  (encodeUtf8 $ pack contentTypeString)
  (toContent $ encodeUtf8 $ pack contentString)

specs :: Spec
specs = describe "typedContentToSnippet" $ do
    it "does not serialize binary content" $ do
      let content = fakeContent "application/octet-stream" "fakefakefake"
      (typedContentToSnippet content 100) `shouldBe` Nothing

    it "serializes UTF-8 encoded JSON" $ do
      let content = fakeContent "application/json; charset=utf-8" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")

    it "truncates long snippets" $ do
      let content = fakeContent "application/json; charset=utf-8" "[1,2,3]"
      (typedContentToSnippet content 2) `shouldBe` (Just "[1...+ 5")
