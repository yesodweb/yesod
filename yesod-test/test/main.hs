{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Test.HUnit hiding (Test)
import Test.Hspec

import Yesod.Core
import Yesod.Form
import Yesod.Test
import Yesod.Test.CssQuery
import Yesod.Test.TransversingCSS
import Text.XML
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Control.Applicative
import Network.Wai (pathInfo, requestHeaders)
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)
import Control.Exception.Lifted(try, SomeException)

import Data.ByteString.Lazy.Char8 ()
import qualified Data.Map as Map
import qualified Text.HTML.DOM as HD
import Network.HTTP.Types.Status (status301, status303, unsupportedMediaType415)

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
        let query = "form.foo input[name=_token][type=hidden][value]"
            html = "<input name='_token' type='hidden' value='foo'><form class='foo'><input name='_token' type='hidden' value='bar'></form>"
            expected = "<input name=\"_token\" type=\"hidden\" value=\"bar\" />"
         in it query $ findBySelector_ html (pack query) @?= [expected]
        it "descendents and children" $
            let html = "<html><p><b><i><u>hello</u></i></b></p></html>"
                query = "p > b u"
             in findBySelector_ html query @?= ["<u>hello</u>"]
        it "hyphenated classes" $
            let html = "<html><p class='foo-bar'><b><i><u>hello</u></i></b></p></html>"
                query = "p.foo-bar u"
             in findBySelector_ html query @?= ["<u>hello</u>"]
        it "descendents" $
            let html = "<html><p><b><i>hello</i></b></p></html>"
                query = "p i"
             in findBySelector_ html query @?= ["<i>hello</i>"]
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
                    addToken
                statusIs 200
                bodyEquals "12345"
            yit "finding html" $ do
                get ("/html" :: Text)
                statusIs 200
                htmlCount "p" 2
                htmlAllContain "p" "Hello"
                htmlAnyContain "p" "World"
                htmlAnyContain "p" "Moon"
                htmlNoneContain "p" "Sun"

        ydescribe "utf8 paths" $ do
            yit "from path" $ do
                get ("/dynamic1/שלום" :: Text)
                statusIs 200
                bodyEquals "שלום"
            yit "from path, type-safe URL" $ do
                get $ LiteAppRoute ["dynamic1", "שלום"]
                statusIs 200
                printBody
                bodyEquals "שלום"
            yit "from WAI" $ do
                get ("/dynamic2/שלום" :: Text)
                statusIs 200
                bodyEquals "שלום"

        ydescribe "labels" $ do
            yit "can click checkbox" $ do
                get ("/labels" :: Text)
                request $ do
                    setMethod "POST"
                    setUrl ("/labels" :: Text)
                    byLabel "Foo Bar" "yes"
        ydescribe "Content-Type handling" $ do
            yit "can set a content-type" $ do
                request $ do
                    setUrl ("/checkContentType" :: Text)
                    addRequestHeader ("Expected-Content-Type","text/plain")
                    addRequestHeader ("Content-Type","text/plain")
                statusIs 200
            yit "adds the form-urlencoded Content-Type if you add parameters" $ do
                request $ do
                    setUrl ("/checkContentType" :: Text)
                    addRequestHeader ("Expected-Content-Type","application/x-www-form-urlencoded")
                    addPostParam "foo" "foobarbaz"
                statusIs 200
            yit "defaults to no Content-Type" $ do
                get ("/checkContentType" :: Text)
                statusIs 200
            yit "returns a 415 for the wrong Content-Type" $ do
                -- Tests that the test handler is functioning
                request $ do
                    setUrl ("/checkContentType" :: Text)
                    addRequestHeader ("Expected-Content-Type","application/x-www-form-urlencoded")
                    addRequestHeader ("Content-Type","text/plain")
                statusIs 415

    describe "cookies" $ yesodSpec cookieApp $ do
        yit "should send the cookie #730" $ do
            get ("/" :: Text)
            statusIs 200
            post ("/cookie/foo" :: Text)
            statusIs 303
            get ("/" :: Text)
            statusIs 200
            printBody
            bodyContains "Foo"
    describe "CSRF with cookies/headers" $ yesodSpec CsrfApp $ do
        yit "Should receive a CSRF cookie and add its value to the headers" $ do
            get ("/" :: Text)
            statusIs 200

            request $ do
                setMethod "POST"
                setUrl ("/" :: Text)
                addTokenFromCookie
            statusIs 200
        yit "Should 403 requests if we don't add the CSRF token" $ do
            get ("/" :: Text)
            statusIs 200

            request $ do
                setMethod "POST"
                setUrl ("/" :: Text)
            statusIs 403
    describe "test redirects" $ yesodSpec app $ do
        yit "follows 303 redirects when requested" $ do
            get ("/redirect303" :: Text)
            statusIs 303
            followRedirect
            statusIs 200
            bodyContains "we have been successfully redirected"

        yit "follows 301 redirects when requested" $ do
            get ("/redirect301" :: Text)
            statusIs 301
            followRedirect
            statusIs 200
            bodyContains "we have been successfully redirected"


        yit "throws an exception when no redirect was returned" $ do
            get ("/" :: Text)
            statusIs 200
            -- This appears to be an HUnitFailure, which is not
            -- exported, so I'm catching SomeException instead.
            (r :: Either SomeException ()) <- try followRedirect
            statusIs 200
            liftIO $ assertBool "expected exception" $ isLeft r

instance RenderMessage LiteApp FormMessage where
    renderMessage _ _ = defaultFormMessage

app :: LiteApp
app = liteApp $ do
    dispatchTo $ do
        mfoo <- lookupGetParam "foo"
        case mfoo of
            Nothing -> return "Hello world!"
            Just foo -> return $ "foo=" <> foo
    onStatic "dynamic1" $ withDynamic $ \d -> dispatchTo $ return (d :: Text)
    onStatic "dynamic2" $ onStatic "שלום" $ dispatchTo $ do
        req <- waiRequest
        return $ pathInfo req !! 1
    onStatic "post" $ dispatchTo $ do
        mfoo <- lookupPostParam "foo"
        case mfoo of
            Nothing -> error "No foo"
            Just foo -> return foo
    onStatic "redirect301" $ dispatchTo $ redirectWith status301 ("/redirectTarget" :: Text) >> return ()
    onStatic "redirect303" $ dispatchTo $ redirectWith status303 ("/redirectTarget" :: Text) >> return ()
    onStatic "redirectTarget" $ dispatchTo $ return ("we have been successfully redirected" :: Text)
    onStatic "form" $ dispatchTo $ do
        ((mfoo, widget), _) <- runFormPost
                        $ renderDivs
                        $ (,)
                      <$> areq textField "Some Label" Nothing
                      <*> areq fileField "Some File" Nothing
        case mfoo of
            FormSuccess (foo, _) -> return $ toHtml foo
            _ -> defaultLayout widget
    onStatic "html" $ dispatchTo $
        return ("<html><head><title>Hello</title></head><body><p>Hello World</p><p>Hello Moon</p></body></html>" :: Text)

    onStatic "labels" $ dispatchTo $
        return ("<html><label><input type='checkbox' name='fooname' id='foobar'>Foo Bar</label></html>" :: Text)

    onStatic "checkContentType" $ dispatchTo $ do
        headers <- requestHeaders <$> waiRequest

        let actual   = lookup "Content-Type" headers
            expected = lookup "Expected-Content-Type" headers

        if actual == expected
            then return ()
            else sendResponseStatus unsupportedMediaType415 ()

cookieApp :: LiteApp
cookieApp = liteApp $ do
    dispatchTo $ fromMaybe "no message available" <$> getMessage
    onStatic "cookie" $ do
        onStatic "foo" $ dispatchTo $ do
            setMessage "Foo"
            redirect ("/cookie/home" :: Text)
            return ()

data CsrfApp = CsrfApp

mkYesod "CsrfApp" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod CsrfApp where
    yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            Welcome to my test application.
    |]

postHomeR :: Handler Html
postHomeR = defaultLayout
    [whamlet|
        <p>
            Welcome to my test application.
    |]
