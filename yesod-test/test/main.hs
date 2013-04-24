{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Test.HUnit hiding (Test)
import Test.Hspec

import Yesod.Core
import Yesod.Form
import Yesod.Test
import Yesod.Test.CssQuery
import Yesod.Test.TransversingCSS
import Text.XML
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Applicative

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
    describe "basic usage" $ yesodSpec app $ do
        ydescribe "tests1" $ do
            yit "tests1a" $ do
                get ("/" :: Text)
                statusIs 200
                bodyEquals "Hello world!"
            yit "tests1b" $ do
                get ("/foo" :: Text)
                statusIs 404
        ydescribe "tests2" $ do
            yit "type-safe URLs" $ do
                get $ LiteAppRoute []
                statusIs 200
            yit "type-safe URLs with query-string" $ do
                get (LiteAppRoute [], [("foo", "bar")])
                statusIs 200
                bodyEquals "foo=bar"
            yit "post params" $ do
                post ("/post" :: Text)
                statusIs 500

                request $ do
                    setMethod "POST"
                    setUrl $ LiteAppRoute ["post"]
                    addPostParam "foo" "foobarbaz"
                statusIs 200
                bodyEquals "foobarbaz"
            yit "labels" $ do
                get ("/form" :: Text)
                statusIs 200

                request $ do
                    setMethod "POST"
                    setUrl ("/form" :: Text)
                    byLabel "Some Label" "12345"
                    fileByLabel "Some File" "test/main.hs" "text/plain"
                    addNonce
                statusIs 200
                bodyEquals "12345"

instance RenderMessage LiteApp FormMessage where
    renderMessage _ _ = defaultFormMessage

app :: LiteApp
app = liteApp $ do
    dispatchTo $ do
        mfoo <- lookupGetParam "foo"
        case mfoo of
            Nothing -> return "Hello world!"
            Just foo -> return $ "foo=" <> foo
    onStatic "post" $ dispatchTo $ do
        mfoo <- lookupPostParam "foo"
        case mfoo of
            Nothing -> error "No foo"
            Just foo -> return foo
    onStatic "form" $ dispatchTo $ do
        ((mfoo, widget), _) <- runFormPost
                        $ renderDivs
                        $ (,)
                      <$> areq textField "Some Label" Nothing
                      <*> areq fileField "Some File" Nothing
        case mfoo of
            FormSuccess (foo, _) -> return $ toHtml foo
            _ -> defaultLayout widget
