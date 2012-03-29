{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (Test)
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()

import Yesod.Test.CssQuery
import Yesod.Test.TransversingCSS
import Yesod.Test.HtmlParse
import Text.XML

import Data.ByteString.Lazy.Char8 ()

parseQuery_ = either error id . parseQuery
findBySelector_ x = either error id . findBySelector x
parseHtml_ = either error id . parseHtml

main :: IO ()
main = hspecX $ do
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
                root = Element "html" []
                    [ NodeElement $ Element "head" []
                        [ NodeElement $ Element "title" []
                            [NodeContent "foo"]
                        ]
                    , NodeElement $ Element "body" []
                        [ NodeElement $ Element "p" []
                            [NodeContent "Hello World"]
                        ]
                    ]
             in parseHtml_ html @?= doc
        it "HTML" $
            let html = "<html><head><title>foo</title></head><body><br><p>Hello World</p></body></html>"
                doc = Document (Prologue [] Nothing []) root []
                root = Element "html" []
                    [ NodeElement $ Element "head" []
                        [ NodeElement $ Element "title" []
                            [NodeContent "foo"]
                        ]
                    , NodeElement $ Element "body" []
                        [ NodeElement $ Element "br" [] []
                        , NodeElement $ Element "p" []
                            [NodeContent "Hello World"]
                        ]
                    ]
             in parseHtml_ html @?= doc
