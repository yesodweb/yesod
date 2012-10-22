{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.CleanPath (cleanPathTest, Widget) where

import Test.Hspec

import Yesod.Core hiding (Request)

import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types (status200, decodePathSegments)

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import Control.Arrow ((***))
import Network.HTTP.Types (encodePath)
import Data.Monoid (mappend)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)

data Subsite = Subsite

getSubsite :: a -> Subsite
getSubsite = const Subsite

instance RenderRoute Subsite where
    data Route Subsite = SubsiteRoute [TS.Text]
        deriving (Eq, Show, Read)
    renderRoute (SubsiteRoute x) = (x, [])

instance YesodDispatch Subsite master where
    yesodDispatch _ _ _ _ _ _ _ pieces _ _ = return $ responseLBS
        status200
        [ ("Content-Type", "SUBSITE")
        ] $ L8.pack $ show pieces

data Y = Y
mkYesod "Y" [parseRoutes|
/foo FooR GET
/foo/#String FooStringR GET
/bar BarR GET
/subsite SubsiteR Subsite getSubsite
/plain PlainR GET
|]

instance Yesod Y where
    approot = ApprootStatic "http://test"
    cleanPath _ s@("subsite":_) = Right s
    cleanPath _ ["bar", ""] = Right ["bar"]
    cleanPath _ ["bar"] = Left ["bar", ""]
    cleanPath _ s =
        if corrected == s
            then Right s
            else Left corrected
      where
        corrected = filter (not . TS.null) s

    joinPath Y ar pieces' qs' =
        fromText ar `mappend` encodePath pieces qs
      where
        pieces = if null pieces' then [""] else pieces'
        qs = map (TE.encodeUtf8 *** go) qs'
        go "" = Nothing
        go x = Just $ TE.encodeUtf8 x

getFooR :: Handler RepPlain
getFooR = return $ RepPlain "foo"

getFooStringR :: String -> Handler RepPlain
getFooStringR = return . RepPlain . toContent

getBarR, getPlainR :: Handler RepPlain
getBarR = return $ RepPlain "bar"
getPlainR = return $ RepPlain "plain"

cleanPathTest :: Spec
cleanPathTest =
  describe "Test.CleanPath" $ do
      it "remove trailing slash" removeTrailingSlash
      it "noTrailingSlash" noTrailingSlash
      it "add trailing slash" addTrailingSlash
      it "has trailing slash" hasTrailingSlash
      it "/foo/something" fooSomething
      it "subsite dispatch" subsiteDispatch
      it "redirect with query string" redQueryString

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

removeTrailingSlash :: IO ()
removeTrailingSlash = runner $ do
    res <- request defaultRequest
                { pathInfo = decodePathSegments "/foo/"
                }
    assertStatus 301 res
    assertHeader "Location" "http://test/foo" res

noTrailingSlash :: IO ()
noTrailingSlash = runner $ do
    res <- request defaultRequest
                { pathInfo = decodePathSegments "/foo"
                }
    assertStatus 200 res
    assertContentType "text/plain; charset=utf-8" res
    assertBody "foo" res

addTrailingSlash :: IO ()
addTrailingSlash = runner $ do
    res <- request defaultRequest
                { pathInfo = decodePathSegments "/bar"
                }
    assertStatus 301 res
    assertHeader "Location" "http://test/bar/" res

hasTrailingSlash :: IO ()
hasTrailingSlash = runner $ do
    res <- request defaultRequest
                { pathInfo = decodePathSegments "/bar/"
                }
    assertStatus 200 res
    assertContentType "text/plain; charset=utf-8" res
    assertBody "bar" res

fooSomething :: IO ()
fooSomething = runner $ do
    res <- request defaultRequest
                { pathInfo = decodePathSegments "/foo/something"
                }
    assertStatus 200 res
    assertContentType "text/plain; charset=utf-8" res
    assertBody "something" res

subsiteDispatch :: IO ()
subsiteDispatch = runner $ do
    res <- request defaultRequest
                { pathInfo = decodePathSegments "/subsite/1/2/3/"
                }
    assertStatus 200 res
    assertContentType "SUBSITE" res
    assertBody "[\"1\",\"2\",\"3\",\"\"]" res

redQueryString :: IO ()
redQueryString = runner $ do
    res <- request defaultRequest
                { pathInfo = decodePathSegments "/plain/"
                , rawQueryString = "?foo=bar"
                }
    assertStatus 301 res
    assertHeader "Location" "http://test/plain?foo=bar" res
