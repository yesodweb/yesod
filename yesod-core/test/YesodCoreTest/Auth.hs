{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses #-}
module YesodCoreTest.Auth
    ( specs
    , Widget
    , resourcesApp
    ) where

import Yesod.Core
import Test.Hspec
import Network.Wai.Test
import Network.Wai
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Data.List (isSuffixOf)
import qualified Network.HTTP.Types as H

data App = App

mkYesod "App" [parseRoutes|
/no-auth NoAuthR
/needs-login-json NeedsLoginJsonR
/needs-login-html NeedsLoginHtmlR
/read-only ReadOnlyR
/forbidden ForbiddenR
|]

instance Yesod App where
    isAuthorized NoAuthR _ = return Authorized
    isAuthorized NeedsLoginJsonR _ = return AuthenticationRequired
    isAuthorized NeedsLoginHtmlR _ = return AuthenticationRequired
    isAuthorized ReadOnlyR False = return Authorized
    isAuthorized ReadOnlyR True = return $ Unauthorized "Read only"
    isAuthorized ForbiddenR _ = return $ Unauthorized "Forbidden"
    authRoute _ = Just NoAuthR

handleNoAuthR, handleReadOnlyR, handleForbiddenR :: Handler ()
handleNoAuthR     = return ()
handleReadOnlyR   = return ()
handleForbiddenR  = return ()

handleNeedsLoginJsonR :: Handler RepJson
handleNeedsLoginJsonR = return $ repJson $ object []
handleNeedsLoginHtmlR :: Handler Html
handleNeedsLoginHtmlR = return ""

test :: String -- ^ method
     -> String -- ^ path
     -> (SResponse -> Session ())
     -> Spec
test method path f = it (method ++ " " ++ path) $ do
    app <- toWaiApp App
    flip runSession app $ do
        sres <- request defaultRequest
            { requestMethod = S8.pack method
            , pathInfo = [T.pack path]
            , requestHeaders =
                if not $ isSuffixOf "json" path then [] else
                  [("Accept", S8.pack "application/json")]
            , httpVersion = H.http11
            }
        f sres

specs :: Spec
specs = describe "Auth" $ do
    test "GET" "no-auth" $ \sres -> assertStatus 200 sres
    test "POST" "no-auth" $ \sres -> assertStatus 200 sres
    test "GET" "needs-login-html" $ \sres -> assertStatus 303 sres
    test "POST" "needs-login-html" $ \sres -> assertStatus 303 sres
    test "GET" "needs-login-json" $ \sres -> assertStatus 401 sres
    test "POST" "needs-login-json" $ \sres -> assertStatus 401 sres
    test "GET" "read-only" $ \sres -> assertStatus 200 sres
    test "POST" "read-only" $ \sres -> assertStatus 403 sres
    test "GET" "forbidden" $ \sres -> assertStatus 403 sres
    test "POST" "forbidden" $ \sres -> assertStatus 403 sres
