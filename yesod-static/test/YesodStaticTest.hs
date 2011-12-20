module YesodStaticTest (specs) where

import Test.Hspec
import Test.HUnit ( (@?=) )
import Test.Hspec.HUnit ( )

import Yesod.Static (getFileListPieces)

specs :: [Specs]
specs = [
    describe "get file list" [
      it "pieces" $ do
        x <- getFileListPieces "test/fs"
        x @?= [["foo"], ["bar", "baz"]]
    ]
  ]
