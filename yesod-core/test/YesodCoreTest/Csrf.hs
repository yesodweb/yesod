{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}

module YesodCoreTest.Csrf (csrfSpec, Widget, resourcesApp) where

import Yesod.Core

import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Web.Cookie
import qualified Data.Map as Map
import Data.ByteString.Lazy (fromStrict)
import Data.Monoid ((<>))

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App where
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

runner :: Session () -> IO ()
runner f = toWaiApp App >>= runSession f

csrfSpec :: Spec
csrfSpec = describe "A Yesod application with the defaultCsrfMiddleware" $ do
    it "serves a includes a cookie in a GET request" $ runner $ do
        res <- request defaultRequest
        assertStatus 200 res
        assertClientCookieExists "Should have an XSRF-TOKEN cookie" defaultCsrfCookieName

    it "200s write requests with the correct CSRF header, but no param" $ runner $ do
        getRes <- request defaultRequest
        assertStatus 200 getRes
        csrfValue <- fmap setCookieValue requireCsrfCookie
        postRes <- request (defaultRequest { requestMethod = "POST", requestHeaders = [(defaultCsrfHeaderName, csrfValue)] })
        assertStatus 200 postRes

    it "200s write requests with the correct CSRF param, but no header" $ runner $ do
        getRes <- request defaultRequest
        assertStatus 200 getRes
        csrfValue <- fmap setCookieValue requireCsrfCookie

        let body = "_token=" <> csrfValue
        postRes <- srequest $ SRequest (defaultRequest { requestMethod = "POST", requestHeaders = [("Content-Type","application/x-www-form-urlencoded")] }) (fromStrict body)
        assertStatus 200 postRes


    it "403s write requests without the CSRF header" $ runner $ do
        res <- request (defaultRequest { requestMethod = "POST" })
        assertStatus 403 res

    it "403s write requests with the wrong CSRF header" $ runner $ do
        getRes <- request defaultRequest
        assertStatus 200 getRes
        csrfValue <- fmap setCookieValue requireCsrfCookie

        res <- request (defaultRequest { requestMethod = "POST", requestHeaders = [(defaultCsrfHeaderName, csrfValue <> "foo")] })
        assertStatus 403 res
        
    it "403s write requests with the wrong CSRF param" $ runner $ do
        getRes <- request defaultRequest
        assertStatus 200 getRes
        csrfValue <- fmap setCookieValue requireCsrfCookie

        let body = "_token=" <> (csrfValue <> "foo")
        postRes <- srequest $ SRequest (defaultRequest { requestMethod = "POST", requestHeaders = [("Content-Type","application/x-www-form-urlencoded")] }) (fromStrict body)
        assertStatus 403 postRes


requireCsrfCookie :: Session SetCookie
requireCsrfCookie = do
    cookies <- getClientCookies
    case Map.lookup defaultCsrfCookieName cookies of
        Just c -> return c
        Nothing -> error "Failed to lookup CSRF cookie"
