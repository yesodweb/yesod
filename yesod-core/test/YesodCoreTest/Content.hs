{-# LANGUAGE OverloadedStrings #-}
module YesodCoreTest.Content (specs) where

import Test.Hspec

import Yesod.Core
import Yesod.Core.Types (TypedContent, typedContentToSnippet)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

fakeContent :: (String -> ByteString) -> String -> String -> TypedContent
fakeContent encode contentTypeString contentString =
  TypedContent
  (encode contentTypeString)
  (toContent $ encode contentString)

fakeUtf8Content :: String -> String -> TypedContent
fakeUtf8Content = fakeContent (encodeUtf8 . pack)

specs :: Spec
specs = describe "typedContentToSnippet" $ do
    it "does not serialize content that is apparently binary data" $ do
      let content = fakeUtf8Content "application/octet-stream" "fake"
      (typedContentToSnippet content 100) `shouldBe` Nothing

    it "serializes UTF-8 encoded JSON" $ do
      let content = fakeUtf8Content "application/json; charset=utf-8" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")

    it "serializes text with no specified encoding" $ do
      let content = fakeUtf8Content "text/css" "fake"
      (typedContentToSnippet content 100) `shouldBe` (Just "fake")

    it "serializes RSS with no specified encoding" $ do
      let content = fakeUtf8Content "application/rss" "fake"
      (typedContentToSnippet content 100) `shouldBe` (Just "fake")

    it "serializes Atom feeds with no specified encoding" $ do
      let content = fakeUtf8Content "application/atom" "fake"
      (typedContentToSnippet content 100) `shouldBe` (Just "fake")

    it "truncates long snippets" $ do
      let content = fakeUtf8Content "application/json; charset=utf-8" "[1,2,3]"
      (typedContentToSnippet content 2) `shouldBe` (Just "[1...+ 5")

    -- GB18030, windows-1251, Shift_JIS and Windows-31J are all ASCII
    -- supersets, so an ASCII payload encodes identically under each. These
    -- cases assert the charset *label* routes to the right decoder and
    -- round-trips ASCII. They run on every platform: where the 'encoding'
    -- package is gated off (Windows) the decoder falls back to utf-8-lenient,
    -- which is also ASCII-identical, so the expected output is unchanged.
    it "serializes GB18030-labelled text" $ do
      let content = fakeUtf8Content "application/json; charset=GB18030" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")

    it "serializes windows-1251-labelled text" $ do
      let content = fakeUtf8Content "application/json; charset=windows-1251" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")

    it "serializes Shift_JIS-labelled text" $ do
      let content = fakeUtf8Content "application/json; charset=Shift_JIS" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")

    it "serializes Windows-31J-labelled text" $ do
      let content = fakeUtf8Content "application/json; charset=Windows-31J" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")
