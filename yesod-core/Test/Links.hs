{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Links (linksTest) where

import Test.Hspec
import Test.Hspec.HUnit

import Yesod.Core hiding (Request)
import Text.Hamlet

import Network.Wai
import Network.Wai.Test

import qualified Data.ByteString.Lazy.Char8 as L8

data Y = Y
mkYesod "Y" [$parseRoutes|
/ RootR GET
|]

instance Yesod Y where
    approot _ = ""

getRootR = defaultLayout $ addHamlet [$hamlet|<a href=@{RootR}>|]

linksTest :: IO [IO Spec]
linksTest = describe "Test.Links"
    [ it "linkToHome" case_linkToHome
    ]

runner f = toWaiApp Y >>= runSession f

case_linkToHome = runner $ do
    res <- request defaultRequest
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><a href=\"/\"></a></body></html>" res
