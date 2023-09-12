-- Ignore warnings about using deprecated byLabel/fileByLabel functions
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    -- avoid warnings
    , resourcesRoutedApp
    , Widget
    ) where

import Test.HUnit hiding (Test)
import Test.Hspec
import qualified Test.Hspec as Hspec

import Yesod.Core
import Yesod.Form
import Yesod.Hspec
import Yesod.Test.CssQuery
import Yesod.Test.TransversingCSS
import Text.XML
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Control.Applicative
import Network.Wai (pathInfo, requestHeaders)
import Network.Wai.Test (SResponse(simpleBody))
import Data.Maybe (fromMaybe)
import Data.Either (isLeft, isRight)

import Data.ByteString.Lazy.Char8 ()
import qualified Data.Map as Map
import qualified Text.HTML.DOM as HD
import Network.HTTP.Types.Status (status200, status301, status303, status403, status422, unsupportedMediaType415)
import UnliftIO.Exception (tryAny, SomeException, try, Exception)
import Control.Monad.IO.Unlift (toIO)
import qualified Web.Cookie as Cookie
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Yesod.Test.Internal (contentTypeHeaderIsUtf8)

parseQuery_ :: Text -> [[SelectorGroup]]
parseQuery_ = either error id . parseQuery

findBySelector_ :: HtmlLBS -> Query -> [String]
findBySelector_ x = either error id . findBySelector x

data RoutedApp = RoutedApp { routedAppInteger :: Integer }

defaultRoutedApp :: RoutedApp
defaultRoutedApp = RoutedApp 0

mkYesod "RoutedApp" [parseRoutes|
/                HomeR      GET POST
/resources       ResourcesR POST
/resources/#Text ResourceR  GET
/get-integer     IntegerR   GET
|]

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
             in HD.parseLBS html @?= doc
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
             in HD.parseLBS html @?= doc
    describe "identifying text-based bodies" $ do
        it "matches content-types with an explicit UTF-8 charset" $ do
            contentTypeHeaderIsUtf8 "application/custom; charset=UTF-8" @?= True
            contentTypeHeaderIsUtf8 "application/custom; charset=utf-8" @?= True
        it "matches content-types with an ASCII charset" $ do
            contentTypeHeaderIsUtf8 "application/custom; charset=us-ascii" @?= True
        it "matches content-types that we assume are UTF-8" $ do
            contentTypeHeaderIsUtf8 "text/html" @?= True
            contentTypeHeaderIsUtf8 "application/json" @?= True
        it "doesn't match content-type headers that are binary data" $ do
            contentTypeHeaderIsUtf8 "image/gif" @?= False
            contentTypeHeaderIsUtf8 "application/pdf" @?= False

    describe "basic usage" $ yesodSpec app $ do
        ydescribe "tests1" $ do
            yit "tests1a" $ do
                get ("/" :: Text)
                statusIs 200
                bodyEquals "Hello world!"
            yit "tests1b" $ do
                get ("/foo" :: Text)
                statusIs 404
            yit "tests1c" $ do
                performMethod "DELETE" ("/" :: Text)
                statusIs 200
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
                    -- If value uses special characters,
                    addPostParam "foo" "foo+bar%41<&baz"
                statusIs 200
                -- They pass through the server correctly.
                bodyEquals "foo+bar%41<&baz"
            yit "labels" $ do
                get ("/form" :: Text)
                statusIs 200

                request $ do
                    setMethod "POST"
                    setUrl ("/form" :: Text)
                    byLabelExact "Some Label" "foo+bar%41<&baz"
                    fileByLabelExact "Some File" "test/main.hs" "text/plain"
                    addToken
                statusIs 200
                -- The '<' and '&' get encoded to HTML entities because
                -- "/form" (unlike "/post") uses toHtml.
                bodyEquals "foo+bar%41&lt;&amp;baz"
            yit "labels WForm" $ do
                get ("/wform" :: Text)
                statusIs 200

                request $ do
                    setMethod "POST"
                    setUrl ("/wform" :: Text)
                    byLabelExact "Some WLabel" "12345"
                    fileByLabelExact "Some WFile" "test/main.hs" "text/plain"
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
            yit "finds the CSRF token by css selector" $ do
                get ("/form" :: Text)
                statusIs 200

                request $ do
                    setMethod "POST"
                    setUrl ("/form" :: Text)
                    byLabelExact "Some Label" "12345"
                    fileByLabelExact "Some File" "test/main.hs" "text/plain"
                    addToken_ "body"
                statusIs 200
                bodyEquals "12345"
            yit "can follow a link via clickOn" $ do
              get ("/htmlWithLink" :: Text)
              clickOn "a#thelink"
              statusIs 200
              bodyEquals "<html><head><title>Hello</title></head><body><p>Hello World</p><p>Hello Moon</p></body></html>"

              get ("/htmlWithLink" :: Text)
              bad <- tryAny (clickOn "a#nonexistentlink")
              assertEq "bad link" (isLeft bad) True

        ydescribe "custom error message" $ do
            yit "returns the message pass to areqMsg" $ do
                get ("/form" :: Text)
                statusIs 200

                request $ do
                    setMethod "POST"
                    setUrl ("/form" :: Text)
                    addToken
                statusIs 200
                htmlAnyContain ".errors" "Missing Label"
            yit "returns the message pass to mreqMsg" $ do
                get ("/mform" :: Text)
                statusIs 200

                request $ do
                    setMethod "POST"
                    setUrl ("/mform" :: Text)
                    addToken
                statusIs 200
                htmlAnyContain ".errors" "Missing MLabel"
            yit "returns the message pass to wreqMsg" $ do
                get ("/wform" :: Text)
                statusIs 200

                request $ do
                    setMethod "POST"
                    setUrl ("/wform" :: Text)
                    addToken
                statusIs 200
                htmlAnyContain ".errors" "Missing WLabel"

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
                    byLabelExact "Foo Bar" "yes"
        ydescribe "byLabel-related tests" $ do
            yit "byLabelExact performs an exact match over the given label name" $ do
                get ("/labels2" :: Text)
                (bad :: Either SomeException ()) <- try (request $ do
                    setMethod "POST"
                    setUrl ("labels2" :: Text)
                    byLabelExact "hobby" "fishing")
                assertEq "failure was called" (isRight bad) True
            yit "byLabelContain looks for the labels which contain the given label name" $ do
                get ("/label-contain" :: Text)
                request $ do
                    setMethod "POST"
                    setUrl ("check-hobby" :: Text)
                    byLabelContain "hobby" "fishing"
                res <- maybe "Couldn't get response" simpleBody <$> getResponse
                assertEq "hobby isn't set" res "fishing"
            yit "byLabelContain throws an error if it finds multiple labels" $ do
                (bad :: Either SomeException ()) <- try (request $ do
                    setMethod "POST"
                    setUrl ("label-contain-error" :: Text)
                    byLabelContain "hobby" "fishing")
                assertEq "failure wasn't called" (isLeft bad) True
            yit "byLabelPrefix matches over the prefix of the labels" $ do
                get ("/label-prefix" :: Text)
                request $ do
                    setMethod "POST"
                    setUrl ("check-hobby" :: Text)
                    byLabelPrefix "hobby" "fishing"
                res <- maybe "Couldn't get response" simpleBody <$> getResponse
                assertEq "hobby isn't set" res "fishing"
            yit "byLabelPrefix throws an error if it finds multiple labels" $ do
                (bad :: Either SomeException ()) <- try (request $ do
                    setMethod "POST"
                    setUrl ("label-prefix-error" :: Text)
                    byLabelPrefix "hobby" "fishing")
                assertEq "failure wasn't called" (isLeft bad) True
            yit "byLabelSuffix matches over the suffix of the labels" $ do
                get ("/label-suffix" :: Text)
                request $ do
                    setMethod "POST"
                    setUrl ("check-hobby" :: Text)
                    byLabelSuffix "hobby" "fishing"
                res <- maybe "Couldn't get response" simpleBody <$> getResponse
                assertEq "hobby isn't set" res "fishing"
            yit "byLabelSuffix throws an error if it finds multiple labels" $ do
                (bad :: Either SomeException ()) <- try (request $ do
                    setMethod "POST"
                    setUrl ("label-suffix-error" :: Text)
                    byLabelSuffix "hobby" "fishing")
                assertEq "failure wasn't called" (isLeft bad) True
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

    describe "hooks" $ yesodSpec app $ do
        ybefore_ (get ("/" :: Text)) $ do
            yit "works" $ do
                statusIs 200
            ybefore_ (post ("/cookie/foo" :: Text)) $ do
                yit "works again" $ do
                    statusIs 404

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
        yit "should 422 on the cookie named key" $ do
            get ("cookie/check-no-cookie" :: Text)
            statusIs 200
            testSetCookie Cookie.defaultSetCookie { Cookie.setCookieName = "key" }
            get ("cookie/check-no-cookie" :: Text)
            statusIs 422
        yit "should be able to delete a cookie" $ do
            testSetCookie Cookie.defaultSetCookie { Cookie.setCookieName = "key" }
            get ("cookie/check-no-cookie" :: Text)
            statusIs 422
            testDeleteCookie "key"
            get ("cookie/check-no-cookie" :: Text)
            statusIs 200
        yit "should be able to clear all cookies" $ do
            testSetCookie Cookie.defaultSetCookie { Cookie.setCookieName = "key" }
            get ("cookie/check-no-cookie" :: Text)
            statusIs 422
            testClearCookies
            get ("cookie/check-no-cookie" :: Text)
            statusIs 200
        yit "should be able to modify cookies" $ do
            testSetCookie Cookie.defaultSetCookie { Cookie.setCookieName = "key" }
            get ("cookie/check-no-cookie" :: Text)
            statusIs 422
            testModifyCookies (\_ -> Map.empty)
            get ("cookie/check-no-cookie" :: Text)
            statusIs 200
    describe "CSRF with cookies/headers" $ yesodSpec defaultRoutedApp $ do
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
            r <- followRedirect
            liftIO $ assertBool "expected a Right from a 303 redirect" $ isRight r
            statusIs 200
            bodyContains "we have been successfully redirected"

        yit "follows 301 redirects when requested" $ do
            get ("/redirect301" :: Text)
            statusIs 301
            r <- followRedirect
            liftIO $ assertBool "expected a Right from a 301 redirect" $ isRight r
            statusIs 200
            bodyContains "we have been successfully redirected"


        yit "returns a Left when no redirect was returned" $ do
            get ("/" :: Text)
            statusIs 200
            r <- followRedirect
            liftIO $ assertBool "expected a Left when not a redirect" $ isLeft r

    describe "route parsing in tests" $ yesodSpec defaultRoutedApp $ do
        yit "parses location header into a route" $ do
            -- get CSRF token
            get HomeR
            statusIs 200

            request $ do
                setMethod "POST"
                setUrl $ ResourcesR
                addPostParam "foo" "bar"
                addTokenFromCookie
            statusIs 201

            loc <- getLocation
            liftIO $ assertBool "expected location to be available" $ isRight loc
            let (Right (ResourceR t)) = loc
            liftIO $ assertBool "expected location header to contain post param" $ t == "bar"

        yit "returns a Left when no redirect was returned" $ do
            get HomeR
            statusIs 200
            loc <- getLocation
            liftIO $ assertBool "expected a Left when not a redirect" $ isLeft loc

    describe "modifying site value" $ yesodSpec defaultRoutedApp $ do
        yit "can change site value" $ do
            get ("/get-integer" :: Text)
            bodyContains "0"
            testModifySite (\site -> pure (site { routedAppInteger = 1 }, id))
            get ("/get-integer" :: Text)
            bodyContains "1"

    describe "Basic Authentication" $ yesodSpec app $ do
        yit "rejects no header" $ do
            get ("checkBasicAuth" :: Text)
            statusIs 403
        yit "rejects incorrect header" $ do
            request $ do
                setUrl ("checkBasicAuth" :: Text)
                addBasicAuthHeader "Aladdin" "foo"
            statusIs 403
        yit "accepts correct header" $ do
            request $ do
                setUrl ("checkBasicAuth" :: Text)
                addBasicAuthHeader "Aladdin" "OpenSesame"
            statusIs 200
    describe "JSON parsing" $ yesodSpec app $ do
        yit "checks for a json array" $ do
            get ("get-json-response" :: Text)
            statusIs 200
            xs <- requireJSONResponse
            assertEq "The value is [1]" xs [1 :: Integer]
        yit "checks for valid content-type" $ do
            get ("get-json-wrong-content-type" :: Text)
            statusIs 200
            (requireJSONResponse :: YesodExample site [Integer]) `liftedShouldThrow` (\(e :: SomeException) -> True)
        yit "checks for valid JSON parse" $ do
            get ("get-json-response" :: Text)
            statusIs 200
            (requireJSONResponse :: YesodExample site [Text]) `liftedShouldThrow` (\(e :: SomeException) -> True)

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
                      Control.Applicative.<$> areqMsg textField "Some Label" ("Missing Label" :: SomeMessage LiteApp) Nothing
                      <*> areq fileField "Some File" Nothing
        case mfoo of
            FormSuccess (foo, _) -> return $ toHtml foo
            _ -> defaultLayout widget
    onStatic "mform" $ dispatchTo $ do
        ((mfoo, widget), _) <- runFormPost $ renderDivs $ formToAForm $ do
          (field1F, field1V) <- mreqMsg textField "Some MLabel" ("Missing MLabel" :: SomeMessage LiteApp) Nothing
          (field2F, field2V) <- mreq fileField "Some MFile" Nothing

          return
            ( (,) Control.Applicative.<$> field1F <*> field2F
            , [field1V, field2V]
            )
        case mfoo of
            FormSuccess (foo, _) -> return $ toHtml foo
            _                    -> defaultLayout widget
    onStatic "wform" $ dispatchTo $ do
        ((mfoo, widget), _) <- runFormPost $ renderDivs $ wFormToAForm $ do
          field1F <- wreqMsg textField "Some WLabel" ("Missing WLabel" :: SomeMessage LiteApp) Nothing
          field2F <- wreq fileField "Some WFile" Nothing

          return $ (,) Control.Applicative.<$> field1F <*> field2F
        case mfoo of
            FormSuccess (foo, _) -> return $ toHtml foo
            _                    -> defaultLayout widget
    onStatic "html" $ dispatchTo $
        return ("<html><head><title>Hello</title></head><body><p>Hello World</p><p>Hello Moon</p></body></html>" :: Text)

    onStatic "htmlWithLink" $ dispatchTo $
        return ("<html><head><title>A link</title></head><body><a href=\"/html\" id=\"thelink\">Link!</a></body></html>" :: Text)
    onStatic "labels" $ dispatchTo $
        return ("<html><label><input type='checkbox' name='fooname' id='foobar'>Foo Bar</label></html>" :: Text)
    onStatic "labels2" $ dispatchTo $
        return ("<html><label for='hobby'>hobby</label><label for='hobby2'>hobby2</label><input type='text' name='hobby' id='hobby'><input type='text' name='hobby2' id='hobby2'></html>" :: Text)
    onStatic "label-contain" $ dispatchTo $
        return ("<html><label for='hobby'>XXXhobbyXXX</label><input type='text' name='hobby' id='hobby'></html>" :: Text)
    onStatic "label-contain-error" $ dispatchTo $
        return ("<html><label for='hobby'>XXXhobbyXXX</label><label for='hobby2'>XXXhobby2XXX</label><input type='text' name='hobby' id='hobby'><input type='text' name='hobby2' id='hobby2'></html>" :: Text)
    onStatic "label-prefix" $ dispatchTo $
        return ("<html><label for='hobby'>hobbyXXX</label><input type='text' name='hobby' id='hobby'></html>" :: Text)
    onStatic "label-prefix-error" $ dispatchTo $
        return ("<html><label for='hobby'>hobbyXXX</label><label for='hobby2'>hobby2XXX</label><input type='text' name='hobby' id='hobby'><input type='text' name='hobby2' id='hobby2'></html>" :: Text)
    onStatic "label-suffix" $ dispatchTo $
        return ("<html><label for='hobby'>XXXhobby</label><input type='text' name='hobby' id='hobby'></html>" :: Text)
    onStatic "label-suffix-error" $ dispatchTo $
        return ("<html><label for='hobby'>XXXhobby</label><label for='hobby2'>XXXneo-hobby</label><input type='text' name='hobby' id='hobby'><input type='text' name='hobby2' id='hobby2'></html>" :: Text)
    onStatic "check-hobby" $ dispatchTo $ do
        hobby <- lookupPostParam "hobby"
        return $ fromMaybe "No hobby" hobby

    onStatic "checkContentType" $ dispatchTo $ do
        headers <- requestHeaders <$> waiRequest

        let actual   = lookup "Content-Type" headers
            expected = lookup "Expected-Content-Type" headers

        if actual == expected
            then return ()
            else sendResponseStatus unsupportedMediaType415 ()
    onStatic "checkBasicAuth" $ dispatchTo $ do
        headers <- requestHeaders <$> waiRequest
        let authHeader = lookup "Authorization" headers

        -- Copied from the Wikipedia Aladdin:OpenSesame example
        if authHeader == Just "Basic QWxhZGRpbjpPcGVuU2VzYW1l"
            then return ()
            else sendResponseStatus status403 ()
    onStatic "get-json-response" $ dispatchTo $ do
        (sendStatusJSON status200 ([1] :: [Integer])) :: LiteHandler Value
    onStatic "get-json-wrong-content-type" $ dispatchTo $ do
        return ("[1]" :: Text)

cookieApp :: LiteApp
cookieApp = liteApp $ do
    dispatchTo $ fromMaybe "no message available" <$> getMessage
    onStatic "cookie" $ do
        onStatic "foo" $ dispatchTo $ do
            setMessage "Foo"
            () <- redirect ("/cookie/home" :: Text)
            return ()
        onStatic "check-no-cookie" $ dispatchTo $ do
            mCookie <- lookupCookie "key"
            if isNothing mCookie
                then return ()
                else sendResponseStatus status422 ()

instance Yesod RoutedApp where
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

postResourcesR :: Handler ()
postResourcesR = do
    t <- runRequestBody >>= \case
        ([("foo", t)], _) -> return t
        _ -> liftIO $ fail "postResourcesR pattern match failure"
    sendResponseCreated $ ResourceR t

getResourceR :: Text -> Handler Html
getResourceR i = defaultLayout
    [whamlet|
        <p>
            Read item #{i}.
    |]

getIntegerR :: Handler Text
getIntegerR = do
    app <- getYesod
    pure $ T.pack $ show (routedAppInteger app)


-- infix Copied from HSpec's version
infix 1 `liftedShouldThrow`

liftedShouldThrow :: (MonadUnliftIO m, HasCallStack, Exception e) => m a -> Hspec.Selector e -> m ()
liftedShouldThrow action sel = do
  ioAction <- toIO action
  liftIO $ ioAction `shouldThrow` sel
