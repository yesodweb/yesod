{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.Links (linksTest, Widget) where

import Test.Hspec
import Test.Hspec.HUnit ()

import Yesod.Core hiding (Request)
import Text.Hamlet
import Network.Wai.Test

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
|]

instance Yesod Y where
    approot _ = ""

getRootR :: Handler RepHtml
getRootR = defaultLayout $ addHamlet [hamlet|<a href=@{RootR}>|]

linksTest :: [Spec]
linksTest = describe "Test.Links"
    [ it "linkToHome" case_linkToHome
    ]

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

case_linkToHome :: IO ()
case_linkToHome = runner $ do
    res <- request defaultRequest
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><a href=\"/\"></a></body></html>" res
