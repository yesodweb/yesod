{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.HUnit hiding (Test)
import Test.Hspec

import Yesod.Test.CssQuery
import Yesod.Test.TransversingCSS
import Text.XML

import Data.ByteString.Lazy.Char8 ()
import qualified Data.Map as Map
import qualified Text.HTML.DOM as HD

parseQuery_ = either error id . parseQuery
findBySelector_ x = either error id . findBySelector x
parseHtml_ = HD.parseLBS

main :: IO ()
main = hspec $ do
    describe "CSS selector parsing" $ do
        it "elements" $ parseQuery_ "strong" @?= [[DeepChildren [ByTagName "strong"]]]
        it "child elements" $ parseQuery_ "strong > i" @?= [[DeepChildren [ByTagName "strong"], DirectChildren [ByTagName "i"]]]
        it "comma" $ parseQuery_ "strong.bar, #foo" @?= [[DeepChildren [ByTagName "strong", ByClass "bar"]], [DeepChildren [ById "foo"]]]
    describe "find by selector" $ do
        it "XHTML" $
            let html = "<html><head><title>foo</title></head><body><p>Hello World</p></body></html>"
                query = "body > p"
             in findBySelector_ html query @?= ["<p>Hello World</p>"]
        it "HTML" $
            let html = "<html><head><title>foo</title></head><body><br><p>Hello World</p></body></html>"
                query = "body > p"
             in findBySelector_ html query @?= ["<p>Hello World</p>"]
    describe "HTML parsing" $ do
        it "XHTML" $
            let html = "<html><head><title>foo</title></head><body><p>Hello World</p></body></html>"
                doc = Document (Prologue [] Nothing []) root []
                root = Element "html" Map.empty
                    [ NodeElement $ Element "head" Map.empty
                        [ NodeElement $ Element "title" Map.empty
                            [NodeContent "foo"]
                        ]
                    , NodeElement $ Element "body" Map.empty
                        [ NodeElement $ Element "p" Map.empty
                            [NodeContent "Hello World"]
                        ]
                    ]
             in parseHtml_ html @?= doc
        it "HTML" $
            let html = "<html><head><title>foo</title></head><body><br><p>Hello World</p></body></html>"
                doc = Document (Prologue [] Nothing []) root []
                root = Element "html" Map.empty
                    [ NodeElement $ Element "head" Map.empty
                        [ NodeElement $ Element "title" Map.empty
                            [NodeContent "foo"]
                        ]
                    , NodeElement $ Element "body" Map.empty
                        [ NodeElement $ Element "br" Map.empty []
                        , NodeElement $ Element "p" Map.empty
                            [NodeContent "Hello World"]
                        ]
                    ]
             in parseHtml_ html @?= doc
