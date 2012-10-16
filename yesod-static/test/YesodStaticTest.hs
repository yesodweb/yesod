module YesodStaticTest (specs) where

import Test.Hspec

import Yesod.Static (getFileListPieces)

specs :: Spec
specs = do
    describe "get file list" $ do
      it "pieces" $ do
        getFileListPieces "test/fs" `shouldReturn` [["foo"], ["bar", "baz"]]
