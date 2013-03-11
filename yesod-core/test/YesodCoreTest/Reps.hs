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

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

specialHtml :: IsString a => a
specialHtml = "text/html; charset=special"

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    let go ct t = provideRepType ct $ return (t :: Text)
    go typeHtml "HTML"
    go specialHtml "HTMLSPECIAL"
    go typeJson "JSON"
    go typeXml "XML"

test :: String -- ^ accept header
     -> ByteString -- ^ expected body
     -> Spec
test accept expected = it accept $ do
    app <- toWaiApp App
    flip runSession app $ do
        sres <- request defaultRequest
            { requestHeaders = [("Accept", S8.pack accept)]
            }
        assertBody expected sres
        assertStatus 200 sres

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
