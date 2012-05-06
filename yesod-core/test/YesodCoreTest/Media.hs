{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module YesodCoreTest.Media (mediaTest, Widget) where

import Test.Hspec
import Test.Hspec.Core (UnevaluatedSpec)
import Test.Hspec.HUnit ()
import Yesod.Core hiding (Request)
import Network.Wai
import Network.Wai.Test
import Text.Lucius
import YesodCoreTest.MediaData

mkYesodDispatch "Y" resourcesY

instance Yesod Y where
    addStaticContent _ _ content = do
        tm <- getRouteToMaster
        route <- getCurrentRoute
        case fmap tm route of
            Just StaticR -> return $ Just $ Left $
                        if content == "foo2{bar:baz}"
                            then "screen.css"
                            else "all.css"
            _ -> return Nothing

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    toWidget [lucius|foo1{bar:baz}|]
    addCassiusMedia "screen" [lucius|foo2{bar:baz}|]
    toWidget [lucius|foo3{bar:baz}|]

getStaticR :: Handler RepHtml
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

mediaTest :: UnevaluatedSpec
mediaTest = describe "Test.Media"
    [ it "media" caseMedia
    , it "media link" caseMediaLink
    ]
