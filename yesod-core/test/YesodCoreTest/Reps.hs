{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ViewPatterns #-}
module YesodCoreTest.Reps (specs, Widget) where

import Yesod.Core
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.String (IsString)
import Data.Text (Text)
import Data.Maybe (fromJust)
import Data.Monoid (Endo (..))
import qualified Control.Monad.Trans.Writer    as Writer
import qualified Data.Set as Set

data App = App

mkYesod "App" [parseRoutes|
/     HomeR GET !home
/json JsonR GET
/parent/#Int ParentR:
    /#Text/child ChildR !child
|]

instance Yesod App

specialHtml :: IsString a => a
specialHtml = "text/html; charset=special"

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    rep typeHtml "HTML"
    rep specialHtml "HTMLSPECIAL"
    rep typeXml "XML"
    rep typeJson "JSON"

rep :: Monad m => ContentType -> Text -> Writer.Writer (Data.Monoid.Endo [ProvidedRep m]) ()
rep ct t = provideRepType ct $ return (t :: Text)

getJsonR :: Handler TypedContent
getJsonR = selectRep $ do
  rep typeHtml "HTML"
  provideRep $ return $ object ["message" .= ("Invalid Login" :: Text)]

handleChildR :: Int -> Text -> Handler ()
handleChildR _ _ = return ()

testRequest :: Int -- ^ http status code
            -> Request
            -> ByteString -- ^ expected body
            -> Spec
testRequest status req expected = it (S8.unpack $ fromJust $ lookup "Accept" $ requestHeaders req) $ do
    app <- toWaiApp App
    flip runSession app $ do
        sres <- request req
        assertStatus status sres
        assertBody expected sres

test :: String -- ^ accept header
     -> ByteString -- ^ expected body
     -> Spec
test accept expected =
    testRequest 200 (acceptRequest accept) expected

acceptRequest :: String -> Request
acceptRequest accept = defaultRequest
            { requestHeaders = [("Accept", S8.pack accept)]
            }

specs :: Spec
specs = do
  describe "selectRep" $ do
    test "application/json" "JSON"
    test (S8.unpack typeJson) "JSON"
    test "text/xml" "XML"
    test (S8.unpack typeXml) "XML"
    test "text/xml,application/json" "XML"
    test "text/xml;q=0.9,application/json;q=1.0" "JSON"
    test (S8.unpack typeHtml) "HTML"
    test "text/html" "HTML"
    test specialHtml "HTMLSPECIAL"
    testRequest 200 (acceptRequest "application/json") { pathInfo = ["json"] } "{\"message\":\"Invalid Login\"}"
    testRequest 406 (acceptRequest "text/foo") "no match found for accept header"
    test "text/*" "HTML"
    test "*/*" "HTML"
  describe "routeAttrs" $ do
    it "HomeR" $ routeAttrs HomeR `shouldBe` Set.singleton "home"
    it "JsonR" $ routeAttrs JsonR `shouldBe` Set.empty
    it "ChildR" $ routeAttrs (ParentR 5 $ ChildR "ignored") `shouldBe` Set.singleton "child"
