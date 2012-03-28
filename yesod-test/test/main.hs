{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (Test)
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()

import Yesod.Test.CssQuery

parseQuery_ = either error id . parseQuery

main :: IO ()
main = hspecX $ do
    describe "CSS selector parsing" $ do
        it "elements" $ parseQuery_ "strong" @?= [[DeepChildren [ByTagName "strong"]]]
        it "child elements" $ parseQuery_ "strong > i" @?= [[DeepChildren [ByTagName "strong"], DirectChildren [ByTagName "i"]]]
        it "comma" $ parseQuery_ "strong.bar, #foo" @?= [[DeepChildren [ByTagName "strong", ByClass "bar"]], [DeepChildren [ById "foo"]]]
