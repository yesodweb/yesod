{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses #-}
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

data App = App

mkYesod "App" [parseRoutes|
/     HomeR GET
/json JsonR GET
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

testRequest :: Request
            -> ByteString -- ^ expected body
            -> Spec
testRequest req expected = it (S8.unpack $ fromJust $ lookup "Accept" $ requestHeaders req) $ do
    app <- toWaiApp App
    flip runSession app $ do
        sres <- request req
        assertBody expected sres
        assertStatus 200 sres

test :: String -- ^ accept header
     -> ByteString -- ^ expected body
     -> Spec
test accept expected =
    testRequest (acceptRequest accept) expected

acceptRequest :: String -> Request
acceptRequest accept = defaultRequest
            { requestHeaders = [("Accept", S8.pack accept)]
            }

specs :: Spec
specs = describe "selectRep" $ do
    test "application/json" "JSON"
    test (S8.unpack typeJson) "JSON"
    test "text/xml" "XML"
    test (S8.unpack typeXml) "XML"
    test "text/xml,application/json" "XML"
    test "text/foo" "HTML"
    test "text/xml;q=0.9,application/json;q=1.0" "JSON"
    test (S8.unpack typeHtml) "HTML"
    test "text/html" "HTML"
    test specialHtml "HTMLSPECIAL"
    testRequest (acceptRequest "application/json") { pathInfo = ["json"] } "{\"message\":\"Invalid Login\"}"
