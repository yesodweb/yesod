{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Widget (widgetTest) where

import Yesod.Core hiding (Request)
import Yesod.Content
import Yesod.Dispatch
import Yesod.Widget
import Text.Julius

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Network.Wai
import Network.Wai.Test

import qualified Data.ByteString.Lazy.Char8 as L8

data Y = Y
mkYesod "Y" [$parseRoutes|
/ RootR GET
/foo/*Strings MultiR GET
/whamlet WhamletR GET
|]

instance Yesod Y where
    approot _ = "http://test"

getRootR = defaultLayout $ addJuliusBody [$julius|<not escaped>|]
getMultiR _ = return ()

data Msg = Hello | Goodbye
instance YesodMessage Y Y where
    type Message Y Y = Msg
    renderMessage _ _ ("en":_) Hello = "Hello"
    renderMessage _ _ ("es":_) Hello = "Hola"
    renderMessage _ _ ("en":_) Goodbye = "Goodbye"
    renderMessage _ _ ("es":_) Goodbye = "Adios"
    renderMessage a b (_:xs) y = renderMessage a b xs y
    renderMessage a b [] y = renderMessage a b ["en"] y

getWhamletR = defaultLayout [$whamlet|
<h1>Test
<h2>@{WhamletR}
<h3>_{Goodbye}
^{embed}
|]
  where
    embed = [$whamlet|<h4>Embed|]

widgetTest :: Test
widgetTest = testGroup "Test.Widget"
    [ testCase "addJuliusBody" case_addJuliusBody
    , testCase "whamlet" case_whamlet
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
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><h1>Test</h1><h2>http://test/whamlet</h2><h3>Adios</h3><h4>Embed</h4></body></html>" res
