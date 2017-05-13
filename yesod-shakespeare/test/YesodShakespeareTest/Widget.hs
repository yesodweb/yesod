{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
module YesodCoreTest.Widget
    ( widgetTest
    , resourcesY
    ) where

import Test.Hspec

import Yesod.Core
import Network.Wai
import Network.Wai.Test

data Y = Y

mkMessage "Y" "test" "en"

type Strings = [String]

mkYesod "Y" [parseRoutes|
/ RootR GET
/foo/*Strings MultiR GET
/whamlet WhamletR GET
/towidget TowidgetR GET
/auto AutoR GET
/jshead JSHeadR GET
|]

instance Yesod Y where
    approot = ApprootStatic "http://test"

getRootR :: Handler Html
getRootR = defaultLayout $ toWidgetBody [julius|<not escaped>|]

getMultiR :: [String] -> Handler ()
getMultiR _ = return ()

data Msg = Hello | Goodbye
instance RenderMessage Y Msg where
    renderMessage _ ("en":_) Hello = "Hello"
    renderMessage _ ("es":_) Hello = "Hola"
    renderMessage _ ("en":_) Goodbye = "Goodbye"
    renderMessage _ ("es":_) Goodbye = "Adios"
    renderMessage a (_:xs) y = renderMessage a xs y
    renderMessage a [] y = renderMessage a ["en"] y

getWhamletR :: Handler Html
getWhamletR = defaultLayout [whamlet|
                    $newline never
                    <h1>Test
                    <h2>@{WhamletR}
                    <h3>_{Goodbye}
                    <h3>_{MsgAnother}
                    ^{embed}
                |]
  where
    embed = [whamlet|
                $newline never
                <h4>Embed
                |]

getAutoR :: Handler Html
getAutoR = defaultLayout [whamlet|
$newline never
^{someHtml}
|]
  where
    someHtml = [shamlet|somehtml|]

getJSHeadR :: Handler Html
getJSHeadR = defaultLayout $ toWidgetHead [julius|alert("hello");|]

getTowidgetR :: Handler Html
getTowidgetR = defaultLayout $ do
    toWidget [julius|toWidget|] :: Widget
    toWidgetHead [julius|toHead|]
    toWidgetBody [julius|toBody|]

    toWidget [lucius|toWidget{bar:baz}|]
    toWidgetHead [lucius|toHead{bar:baz}|]

    toWidget [hamlet|<p>toWidget|]
    toWidgetHead [hamlet|<toHead>|]
    toWidgetBody [hamlet|<p>toBody|]

widgetTest :: Spec
widgetTest = describe "Test.Widget" $ do
  it "addJuliusBody" case_addJuliusBody
  it "whamlet" case_whamlet
  it "two letter lang codes" case_two_letter_lang
  it "automatically applies toWidget" case_auto
  it "toWidgetHead puts JS in head" case_jshead
  it "toWidget" $ runner $ do
    res <- request defaultRequest
        { pathInfo = ["towidget"]
        }
    assertBody "<!DOCTYPE html>\n<html><head><title></title><script>toHead</script><toHead></toHead>\n<style>toWidget{bar:baz}toHead{bar:baz}</style></head><body><script>toBody</script><p>toWidget</p>\n<p>toBody</p>\n<script>toWidget</script></body></html>" res

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

case_addJuliusBody :: IO ()
case_addJuliusBody = runner $ do
    res <- request defaultRequest
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><script><not escaped></script></body></html>" res

case_whamlet :: IO ()
case_whamlet = runner $ do
    res <- request defaultRequest
        { pathInfo = ["whamlet"]
        , requestHeaders = [("Accept-Language", "es")]
        }
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><h1>Test</h1><h2>http://test/whamlet</h2><h3>Adios</h3><h3>String</h3><h4>Embed</h4></body></html>" res

case_two_letter_lang :: IO ()
case_two_letter_lang = runner $ do
    res <- request defaultRequest
        { pathInfo = ["whamlet"]
        , requestHeaders = [("Accept-Language", "es-ES")]
        }
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><h1>Test</h1><h2>http://test/whamlet</h2><h3>Adios</h3><h3>String</h3><h4>Embed</h4></body></html>" res

case_auto :: IO ()
case_auto = runner $ do
    res <- request defaultRequest
        { pathInfo = ["auto"]
        , requestHeaders = [("Accept-Language", "es")]
        }
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body>somehtml</body></html>" res

case_jshead :: IO ()
case_jshead = runner $ do
    res <- request defaultRequest
        { pathInfo = ["jshead"]
        }
    assertBody "<!DOCTYPE html>\n<html><head><title></title><script>alert(\"hello\");</script></head><body></body></html>" res
    assertHeader "Vary" "Accept, Accept-Language" res
