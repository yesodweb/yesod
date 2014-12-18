{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
module YesodCoreTest.Ssl ( sslOnlySpec, unsecSpec ) where
import qualified YesodCoreTest.StubSslOnly as Ssl
import qualified YesodCoreTest.StubUnsecured as Unsecured
import Yesod.Core
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import qualified Data.ByteString.Char8 as C8
import qualified Web.Cookie as Cookie
import qualified Data.List as DL

type CookieSpec = Cookie.SetCookie -> Bool

type ResponseExpectation = SResponse -> Session ()

homeFixtureFor :: YesodDispatch a => a -> ResponseExpectation -> IO ()
homeFixtureFor app assertion = do
    wa <- toWaiApp app
    runSession (getHome >>= assertion) wa
  where
    getHome = request defaultRequest

cookieShouldSatisfy :: String -> CookieSpec -> ResponseExpectation
cookieShouldSatisfy name spec response =
    liftIO $
      case DL.filter matchesName $ cookiesIn response of
          [] -> expectationFailure $ DL.concat
            [ "Expected a cookie named "
            , name
            , " but none is set"
            ]
          [c] -> c `shouldSatisfy` spec
          _ -> expectationFailure $ DL.concat
            [ "Expected one cookie named "
            , name
            , " but found more than one"
            ]
  where
    matchesName c = (Cookie.setCookieName c) == C8.pack name
    cookiesIn r =
      DL.map
        (Cookie.parseSetCookie . snd)
        (DL.filter (("Set-Cookie"==) . fst) $ simpleHeaders r)

sslOnlySpec :: Spec
sslOnlySpec = describe "A Yesod application with sslOnly on" $ do
    it "serves a Strict-Transport-Security header in all responses" $
        atHome $ assertHeader "Strict-Transport-Security"
                              "max-age=7200; includeSubDomains"
    it "sets the Secure flag on its session cookie" $
        atHome $ "_SESSION" `cookieShouldSatisfy` Cookie.setCookieSecure
  where
    atHome = homeFixtureFor Ssl.App

unsecSpec :: Spec
unsecSpec = describe "A Yesod application with sslOnly off" $ do
    it "never serves a Strict-Transport-Security header" $ do
        atHome $ assertNoHeader "Strict-Transport-Security"
    it "does not set the Secure flag on its session cookie" $ do
        atHome $ "_SESSION" `cookieShouldSatisfy` isNotSecure
  where
    atHome = homeFixtureFor Unsecured.App
    isNotSecure c = not $ Cookie.setCookieSecure c
