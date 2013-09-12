{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}
module EmbedDevelTest where

-- Tests the development mode of the embedded static subsite by
-- using a custom generator testGen.

import Data.Maybe (isNothing)
import EmbedTestGenerator
import EmbedProductionTest (findEtag)
import Network.Wai.Test (SResponse(simpleHeaders))
import Test.HUnit (assertBool)
import Test.Hspec (Spec)
import Yesod.Core
import Yesod.EmbeddedStatic
import Yesod.Test

mkEmbeddedStatic True "eDev" [testGen]

data MyApp = MyApp { getStatic :: EmbeddedStatic }

mkYesod "MyApp" [parseRoutes|
/static StaticR EmbeddedStatic getStatic
|]

instance Yesod MyApp

noCacheControl :: YesodExample site ()
noCacheControl = withResponse $ \r -> do
    liftIO $ assertBool "Cache-Control exists" $
        isNothing $ lookup "Cache-Control" $ simpleHeaders r
    liftIO $ assertBool "Expires exists" $
        isNothing $ lookup "Expires" $ simpleHeaders r

embedDevSpecs :: Spec
embedDevSpecs = yesodSpec (MyApp eDev) $ do
    ydescribe "Embedded Development Entries" $ do
        yit "e1 loads" $ do
            get $ StaticR e1
            statusIs 200
            assertHeader "Content-Type" "text/plain"
            noCacheControl
            bodyEquals "e1 devel"

            tag <- findEtag
            request $ do
                setMethod "GET"
                setUrl $ StaticR e1
                addRequestHeader ("If-None-Match", tag)
            statusIs 304

        yit "e2 with simulated directory" $ do
            get $ StaticR e2
            statusIs 200
            assertHeader "Content-Type" "abcdef"
            noCacheControl
            bodyEquals "e2 devel"

        yit "e3 without haskell name" $ do
            get $ StaticR $ embeddedResourceR ["xxxx", "e3"] []
            statusIs 200
            assertHeader "Content-Type" "yyy"
            noCacheControl
            bodyEquals "e3 devel"

        yit "e4 loads" $ do
            get $ StaticR e4
            statusIs 200
            assertHeader "Content-Type" "text/plain"
            noCacheControl
            bodyEquals "e4 devel"

        yit "e4 extra development dev1" $ do
            get $ StaticR $ embeddedResourceR ["dev1"] []
            statusIs 200
            assertHeader "Content-Type" "mime"
            noCacheControl
            bodyEquals "dev1 content"

            tag <- findEtag
            request $ do
                setMethod "GET"
                setUrl $ StaticR $ embeddedResourceR ["dev1"] []
                addRequestHeader ("If-None-Match", tag)
            statusIs 304

        yit "e4 extra development with path" $ do
            get $ StaticR $ embeddedResourceR ["dir", "dev2"] []
            statusIs 200
            assertHeader "Content-Type" "mime2"
            noCacheControl
            bodyEquals "dev2 content"

        yit "extra development file 404" $ do
            get $ StaticR $ embeddedResourceR ["xxxxxxxxxx"] []
            statusIs 404
