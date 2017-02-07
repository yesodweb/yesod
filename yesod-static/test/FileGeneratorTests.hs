{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module FileGeneratorTests (fileGenSpecs) where

import Control.Exception
import Control.Monad (forM_)
import GeneratorTestUtil
import Test.Hspec
import Test.HUnit (assertFailure, assertEqual)
import Yesod.EmbeddedStatic.Generators
import qualified Data.ByteString.Lazy as BL

-- | Embeds the LICENSE file
license :: GenTestResult
license = $(embedFile "LICENSE" >>= 
            testOneEntry (Just "_LICENSE") "LICENSE" (BL.readFile "LICENSE")
           )

licenseAt :: GenTestResult
licenseAt = $(embedFileAt "abc.txt" "LICENSE" >>=
              testOneEntry (Just "abc_txt") "abc.txt" (BL.readFile "LICENSE")
             )

embDir :: [GenTestResult]
embDir = $(embedDir "test/embed-dir" >>=
           testEntries 
            [ (Just "abc_def_txt", "abc/def.txt", BL.readFile "test/embed-dir/abc/def.txt")
            , (Just "lorem_txt", "lorem.txt", BL.readFile "test/embed-dir/lorem.txt")
            , (Just "foo", "foo", BL.readFile "test/embed-dir/foo")
            ]
          )

embDirAt :: [GenTestResult]
embDirAt = $(embedDirAt "xxx" "test/embed-dir" >>=
           testEntries 
            [ (Just "xxx_abc_def_txt", "xxx/abc/def.txt", BL.readFile "test/embed-dir/abc/def.txt")
            , (Just "xxx_lorem_txt", "xxx/lorem.txt", BL.readFile "test/embed-dir/lorem.txt")
            , (Just "xxx_foo", "xxx/foo", BL.readFile "test/embed-dir/foo")
            ]
          )

concatR :: GenTestResult
concatR = $(concatFiles "out.txt" [ "test/embed-dir/abc/def.txt", "test/embed-dir/foo"] >>=
            testOneEntry (Just "out_txt") "out.txt" (return "Yesod Rocks\nBar\n")
           )

-- The transform function should only run at compile for the production content
concatWithR :: GenTestResult
concatWithR = $(concatFilesWith "out2.txt"
                                (\x -> return $ x `BL.append` "Extra")
                                [ "test/embed-dir/abc/def.txt", "test/embed-dir/foo"] >>=
                testOneEntry (Just "out2_txt") "out2.txt" (return "Yesod Rocks\nBar\nExtra")
               )
            
fileGenSpecs :: Spec
fileGenSpecs = do
    describe "Embed File" $ do
        it "embeds a single file" $
            assertGenResult (BL.readFile "LICENSE") license
        it "embeds a single file at a location" $
            assertGenResult (BL.readFile "LICENSE") licenseAt

    describe "Embed Directory" $ do
        it "embeds a directory" $
            forM_ [embDir, embDirAt] $ \d -> case d of
                [GenError e] -> assertFailure e
                [def, foo, lorem] -> do
                    assertGenResult (BL.readFile "test/embed-dir/abc/def.txt") def
                    assertGenResult (BL.readFile "test/embed-dir/foo") foo
                    assertGenResult (BL.readFile "test/embed-dir/lorem.txt") lorem
                _ -> assertFailure "Bad directory list"

    describe "Concat Files" $ do
        it "simple concat" $
            assertGenResult (return "Yesod Rocks\nBar\n") concatR
        it "concat with processing function" $
            assertGenResult (return "Yesod Rocks\nBar\n") concatWithR -- no Extra since this is development

    describe "Compress" $ do
        it "compress tool function" $ do
            out <- compressTool "runhaskell" [] "main = putStrLn \"Hello World\""
            -- 13 == CR, to make this test work on Windows
            BL.filter (/= 13) out `shouldBe` "Hello World\n"

        it "tryCompressTools" $ do
            out <- flip tryCompressTools "abcdef" 
                            [ const $ throwIO $ ErrorCall "An expected error"
                            , const $ return "foo"
                            , const $ return "bar"
                            ]
            assertEqual "" "foo" out
            out2 <- flip tryCompressTools "abcdef"
                            [ const $ throwIO $ ErrorCall "An expected error"]
            assertEqual "" "abcdef" out2
