{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Widget (widgetTest) where

import Test.Hspec
import Test.Hspec.HUnit

import Yesod.Core hiding (Request)
import Yesod.Content
import Yesod.Dispatch
import Yesod.Widget
import Text.Julius
import Text.Lucius
import Text.Hamlet

import Network.Wai
import Network.Wai.Test

import qualified Data.ByteString.Lazy.Char8 as L8

data Y = Y

mkMessage "Y" "test" "en"

type Strings = [String]

mkYesod "Y" [$parseRoutes|
/ RootR GET
/foo/*Strings MultiR GET
/whamlet WhamletR GET
/towidget TowidgetR GET
|]

instance Yesod Y where
    approot _ = "http://test"

getRootR = defaultLayout $ toWidgetBody [$julius|<not escaped>|]
getMultiR _ = return ()

data Msg = Hello | Goodbye
instance RenderMessage Y Msg where
    renderMessage _ ("en":_) Hello = "Hello"
    renderMessage _ ("es":_) Hello = "Hola"
    renderMessage _ ("en":_) Goodbye = "Goodbye"
    renderMessage _ ("es":_) Goodbye = "Adios"
    renderMessage a (_:xs) y = renderMessage a xs y
    renderMessage a [] y = renderMessage a ["en"] y

getTowidgetR :: Handler RepHtml
getTowidgetR = defaultLayout $ do
    toWidget [julius|foo|]
    toWidgetHead [julius|foo|]
    toWidgetBody [julius|foo|]

    toWidget [lucius|foo{bar:baz}|]
    toWidgetHead [lucius|foo{bar:baz}|]

    toWidget [hamlet|<foo>|] :: Widget
    toWidgetHead [hamlet|<foo>|]
    toWidgetBody [hamlet|<foo>|]

getWhamletR = defaultLayout [$whamlet|
<h1>Test
<h2>@{WhamletR}
<h3>_{Goodbye}
<h3>_{MsgAnother}
^{embed}
|]
  where
    embed = [$whamlet|<h4>Embed|]

widgetTest :: IO [IO Spec]
widgetTest = describe "Test.Widget"
    [ it "addJuliusBody" case_addJuliusBody
    , it "whamlet" case_whamlet
    ]

runner f = toWaiApp Y >>= runSession f
defaultRequest = Request
    { pathInfo = []
    , requestHeaders = []
    , queryString = []
    , requestMethod = "GET"
    }

case_addJuliusBody = runner $ do
    res <- request defaultRequest
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><script><not escaped></script></body></html>" res

case_whamlet = runner $ do
    res <- request defaultRequest
        { pathInfo = ["whamlet"]
        , requestHeaders = [("Accept-Language", "es")]
        }
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><h1>Test</h1><h2>http://test/whamlet</h2><h3>Adios</h3><h3>String</h3><h4>Embed</h4></body></html>" res
