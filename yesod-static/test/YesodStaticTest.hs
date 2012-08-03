module YesodStaticTest (specs) where

import Test.Hspec
import Test.HUnit ( (@?=) )
import Test.Hspec.HUnit ( )

import Yesod.Static (getFileListPieces)

specs :: Spec
specs = do
    describe "get file list" $ do
      it "pieces" $ do
        x <- getFileListPieces "test/fs"
        x @?= [["foo"], ["bar", "baz"]]
