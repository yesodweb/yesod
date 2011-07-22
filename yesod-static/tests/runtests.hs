{-# LANGUAGE OverloadedStrings #-}
import Yesod.Static

import Test.Hspec
import Test.Hspec.HUnit ()
-- import Test.Hspec.QuickCheck (prop)
import Test.HUnit ((@?=))

main :: IO ()
main = hspecX $ return [] {- FIXME specs

specs :: IO [Spec]
specs = runSpecM $ do
  context "get file list" $ do
    ti "pieces" $ do
      x <- getFileListPieces "tests/data"
      x @?= [["foo"], ["bar", "baz"]]-}
