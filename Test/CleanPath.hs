{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.CleanPath (cleanPathTest) where

import Yesod.Core
import Yesod.Content
import Yesod.Dispatch

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Network.Wai
import Network.Wai.Test

data Y = Y
mkYesod "Y" [$parseRoutes|
/foo FooR GET
/foo/#String FooStringR GET
/bar BarR GET
|]

instance Yesod Y where
    approot _ = "http://test"
    cleanPath _ ["bar", ""] = Right ["bar"]
    cleanPath _ ["bar"] = Left ["bar", ""]
    cleanPath _ s =
        if corrected == s
            then Right s
            else Left corrected
      where
        corrected = filter (not . null) s

getFooR = return $ RepPlain "foo"
getFooStringR = return . RepPlain . toContent
getBarR = return $ RepPlain "bar"

cleanPathTest :: Test
cleanPathTest = testGroup "Test.CleanPath"
    [ testCase "remove trailing slash" removeTrailingSlash
    , testCase "noTrailingSlash" noTrailingSlash
    , testCase "add trailing slash" addTrailingSlash
    , testCase "has trailing slash" hasTrailingSlash
    , testCase "/foo/something" fooSomething
    ]

runner f = toWaiApp Y >>= runSession f
defaultRequest = Request
    { pathInfo = ""
    , requestHeaders = []
    , queryString = ""
    , requestMethod = "GET"
    }

removeTrailingSlash = runner $ do
    res <- request defaultRequest
                { pathInfo = "/foo/"
                }
    assertStatus 301 res
    assertHeader "Location" "http://test/foo" res

noTrailingSlash = runner $ do
    res <- request defaultRequest
                { pathInfo = "/foo"
                }
    assertStatus 200 res
    assertContentType "text/plain; charset=utf-8" res
    assertBody "foo" res

addTrailingSlash = runner $ do
    res <- request defaultRequest
                { pathInfo = "/bar"
                }
    assertStatus 301 res
    assertHeader "Location" "http://test/bar/" res

hasTrailingSlash = runner $ do
    res <- request defaultRequest
                { pathInfo = "/bar/"
                }
    assertStatus 200 res
    assertContentType "text/plain; charset=utf-8" res
    assertBody "bar" res

fooSomething = runner $ do
    res <- request defaultRequest
                { pathInfo = "/foo/something"
                }
    assertStatus 200 res
    assertContentType "text/plain; charset=utf-8" res
    assertBody "something" res
