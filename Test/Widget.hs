{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Widget (widgetTest) where

import Yesod.Core
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
|]

instance Yesod Y where
    approot _ = "http://test"

getRootR = defaultLayout $ addJuliusBody [$julius|<not escaped>|]

widgetTest :: Test
widgetTest = testGroup "Test.Exceptions"
    [ testCase "addJuliusBody" case_addJuliusBody
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
