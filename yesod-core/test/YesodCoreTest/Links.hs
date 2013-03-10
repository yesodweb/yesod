{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.Links (linksTest, Widget) where

import Test.Hspec

import Yesod.Core
import Text.Hamlet
import Network.Wai
import Network.Wai.Test
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Blaze.ByteString.Builder (toByteString)

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/single/#Text TextR GET
/multi/*Texts TextsR GET
|]

instance Yesod Y

getRootR :: Handler RepHtml
getRootR = defaultLayout $ toWidget [hamlet|<a href=@{RootR}>|]

getTextR :: Text -> Handler RepHtml
getTextR foo = defaultLayout $ toWidget [hamlet|%#{foo}%|]

getTextsR :: [Text] -> Handler RepHtml
getTextsR foos = defaultLayout $ toWidget [hamlet|%#{show foos}%|]

linksTest :: Spec
linksTest = describe "Test.Links" $ do
      it "linkToHome" case_linkToHome
      it "blank path pieces" case_blanks

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

case_linkToHome :: IO ()
case_linkToHome = runner $ do
    res <- request defaultRequest
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><a href=\"/\"></a>\n</body></html>" res

case_blanks :: IO ()
case_blanks = runner $ do
    liftIO $ do
        let go r =
                let (ps, qs) = renderRoute r
                 in toByteString $ joinPath Y "" ps qs
        (go $ TextR "-") `shouldBe` "/single/--"
        (go $ TextR "") `shouldBe` "/single/-"
        (go $ TextsR ["", "-", "foo", "", "bar"]) `shouldBe` "/multi/-/--/foo/-/bar"

    res1 <- request defaultRequest
        { pathInfo = ["single", "-"]
        , rawPathInfo = "dummy1"
        }
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body>%%</body></html>" res1

    res2 <- request defaultRequest
        { pathInfo = ["multi", "foo", "-", "bar"]
        , rawPathInfo = "dummy2"
        }
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body>%[&quot;foo&quot;,&quot;&quot;,&quot;bar&quot;]%</body></html>" res2
