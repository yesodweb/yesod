{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module YesodCoreTest.Media (mediaTest, Widget) where

import Test.Hspec
import Yesod.Core
import Network.Wai
import Network.Wai.Test
import Text.Lucius
import YesodCoreTest.MediaData

mkYesodDispatch "Y" resourcesY

instance Yesod Y where
    addStaticContent _ _ content = do
        route <- getCurrentRoute
        case route of
            Just StaticR -> return $ Just $ Left $
                        if content == "foo2{bar:baz}"
                            then "screen.css"
                            else "all.css"
            _ -> return Nothing

getRootR :: Handler Html
getRootR = defaultLayout $ do
    toWidget [lucius|foo1{bar:baz}|]
    toWidgetMedia "screen" [lucius|foo2{bar:baz}|]
    toWidget [lucius|foo3{bar:baz}|]

getStaticR :: Handler Html
getStaticR = getRootR

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

caseMedia :: IO ()
caseMedia = runner $ do
    res <- request defaultRequest
    assertStatus 200 res
    flip assertBody res "<!DOCTYPE html>\n<html><head><title></title><style>foo1{bar:baz}foo3{bar:baz}</style><style media=\"screen\">foo2{bar:baz}</style></head><body></body></html>"

caseMediaLink :: IO ()
caseMediaLink = runner $ do
    res <- request defaultRequest { pathInfo = ["static"] }
    assertStatus 200 res
    flip assertBody res "<!DOCTYPE html>\n<html><head><title></title><link rel=\"stylesheet\" href=\"all.css\"><link rel=\"stylesheet\" media=\"screen\" href=\"screen.css\"></head><body></body></html>"

mediaTest :: Spec
mediaTest = describe "Test.Media" $ do
      it "media" caseMedia
      it "media link" caseMediaLink
