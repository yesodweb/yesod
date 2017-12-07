{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
module YesodCoreTest.Links
    ( linksTest
    , Widget
    , resourcesY
    ) where

import Test.Hspec

import Yesod.Core
import Network.Wai
import Network.Wai.Test
import Data.Text (Text)
import Blaze.ByteString.Builder (toByteString)

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/single/#Text TextR GET
/multi/*Texts TextsR GET

/route-test-1/+[Text] RT1 GET
/route-test-2/*Vector-String RT2 GET
/route-test-3/*Vector-(Maybe-Int) RT3 GET
/route-test-4/#(Foo-Int-Int) RT4 GET
/route-test-4-spaces/#{Foo Int Int} RT4Spaces GET
|]

data Vector a = Vector
    deriving (Show, Read, Eq)

instance PathMultiPiece (Vector a) where
    toPathMultiPiece = error "toPathMultiPiece"
    fromPathMultiPiece = error "fromPathMultiPiece"

data Foo x y = Foo
    deriving (Show, Read, Eq)

instance PathPiece (Foo x y) where
    toPathPiece = error "toPathPiece"
    fromPathPiece = error "fromPathPiece"

instance Yesod Y

getRootR :: Handler Html
getRootR = defaultLayout $ toWidget [hamlet|<a href=@{RootR}>|]

getTextR :: Text -> Handler Html
getTextR foo = defaultLayout $ toWidget [hamlet|%#{foo}%|]

getTextsR :: [Text] -> Handler Html
getTextsR foos = defaultLayout $ toWidget [hamlet|%#{show foos}%|]

getRT1 :: [Text] -> Handler ()
getRT1 _ = return ()

getRT2 :: Vector String -> Handler ()
getRT2 _ = return ()

getRT3 :: Vector (Maybe Int) -> Handler ()
getRT3 _ = return ()

getRT4 :: Foo Int Int -> Handler ()
getRT4 _ = return ()

getRT4Spaces :: Foo Int Int -> Handler ()
getRT4Spaces _ = return ()

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
