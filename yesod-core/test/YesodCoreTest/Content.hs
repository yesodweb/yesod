{-# LANGUAGE OverloadedStrings #-}
module YesodCoreTest.Content (specs) where

import Test.Hspec

import Yesod.Core
import Yesod.Core.Content
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

import qualified Data.Encoding as Enc
import qualified Data.Encoding.GB18030 as Enc
import qualified Data.Encoding.CP1251 as Enc
import qualified Data.Encoding.ShiftJIS as Enc
import qualified Data.Encoding.CP932 as Enc

fakeContent :: (String -> ByteString) -> String -> String -> TypedContent
fakeContent encode contentTypeString contentString =
  TypedContent
  (encode contentTypeString)
  (toContent $ encode contentString)

fakeUtf8Content :: String -> String -> TypedContent
fakeUtf8Content = fakeContent (encodeUtf8 . pack)

fakeGB18030Content :: String -> String -> TypedContent
fakeGB18030Content = fakeContent $ Enc.encodeStrictByteString Enc.GB18030

fakeCP1251Content :: String -> String -> TypedContent
fakeCP1251Content = fakeContent $ Enc.encodeStrictByteString Enc.CP1251

fakeShiftJISContent :: String -> String -> TypedContent
fakeShiftJISContent = fakeContent $ Enc.encodeStrictByteString Enc.ShiftJIS

fakeCP932Content :: String -> String -> TypedContent
fakeCP932Content = fakeContent $ Enc.encodeStrictByteString Enc.CP932

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

    it "serializes GB18030 text" $ do
      let content = fakeGB18030Content "application/json; charset=GB18030" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")

    it "serializes CP1251 text" $ do
      let content = fakeCP1251Content "application/json; charset=windows-1251" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")

    it "serializes ShiftJIS text" $ do
      let content = fakeShiftJISContent "application/json; charset=Shift_JIS" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")

    it "serializes CP932 text" $ do
      let content = fakeCP932Content "application/json; charset=Windows-31J" "[1,2,3]"
      (typedContentToSnippet content 100) `shouldBe` (Just "[1,2,3]")
