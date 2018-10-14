{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module EmbedProductionTest where

-- Tests the production mode of the embedded static subsite by
-- using a custom generator testGen.  Also tests that the widget
-- content is embedded properly.

import Data.Maybe (isJust)
import EmbedTestGenerator
import Network.Wai.Test (SResponse(simpleHeaders))
import Test.HUnit (assertFailure, assertBool)
import Test.Hspec (Spec)
import Yesod.Core
import Yesod.EmbeddedStatic
import Yesod.Test
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

mkEmbeddedStatic False "eProduction" [testGen]

data MyApp = MyApp { getStatic :: EmbeddedStatic }

mkYesod "MyApp" [parseRoutes|
/ HomeR GET
/static StaticR EmbeddedStatic getStatic
|]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    toWidget [julius|console.log("Hello World");|]
    [whamlet|<h1>Hello|]

instance Yesod MyApp where
     addStaticContent = embedStaticContent getStatic StaticR Right

findEtag :: YesodExample site B.ByteString
findEtag = withResponse $ \r ->
    case lookup "ETag" (simpleHeaders r) of
        Nothing -> liftIO (assertFailure "No etag found") >> error ""
        Just e -> return e

hasCacheControl :: YesodExample site ()
hasCacheControl = withResponse $ \r -> do
    liftIO $ assertBool "Cache-Control missing" $
        isJust $ lookup "Cache-Control" $ simpleHeaders r
    liftIO $ assertBool "Expires missing" $
        isJust $ lookup "Expires" $ simpleHeaders r

embedProductionSpecs :: Spec
embedProductionSpecs = yesodSpec (MyApp eProduction) $ do
    ydescribe "Embedded Production Entries" $ do
        yit "e1 loads" $ do
            get $ StaticR e1
            statusIs 200
            assertHeader "Content-Type" "text/plain"
            hasCacheControl
            bodyEquals "e1 production"

            tag <- findEtag
            request $ do
                setMethod "GET"
                setUrl $ StaticR e1
                addRequestHeader ("If-None-Match", tag)
            statusIs 304

        yit "e1 with custom built path" $ do
            get $ StaticR $ embeddedResourceR ["e1"] []
            statusIs 200
            assertHeader "Content-Type" "text/plain"
            hasCacheControl
            bodyEquals "e1 production"

        yit "e2 with simulated directory" $ do
            get $ StaticR e2
            statusIs 200
            assertHeader "Content-Type" "abcdef"
            hasCacheControl
            bodyEquals "e2 production"

        yit "e2 with custom built directory path" $ do
            get $ StaticR $ embeddedResourceR ["dir", "e2"] []
            statusIs 200
            assertHeader "Content-Type" "abcdef"
            hasCacheControl
            bodyEquals "e2 production"

        yit "e3 without haskell name" $ do
            get $ StaticR $ embeddedResourceR ["xxxx", "e3"] []
            statusIs 200
            assertHeader "Content-Type" "yyy"
            hasCacheControl
            bodyEquals "e3 production"

        yit "e4 is embedded" $ do
            get $ StaticR e4
            statusIs 200
            assertHeader "Content-Type" "text/plain"
            hasCacheControl
            bodyEquals "e4 production"

        yit "e4 extra development files are not embedded" $ do
            get $ StaticR $ embeddedResourceR ["dev1"] []
            statusIs 404

    ydescribe "Embedded Widget Content" $
        yit "Embedded Javascript" $ do
            get HomeR
            statusIs 200
            script <- htmlQuery "script" >>= \case
                [s] -> return s
                _ -> liftIO $ fail "Expected singleton list of script"
            let src = BL.takeWhile (/= 34) $ BL.drop 1 $ BL.dropWhile (/= 34) script -- 34 is "

            get $ TL.toStrict $ TL.decodeUtf8 src
            statusIs 200
            hasCacheControl
            assertHeader "Content-Type" "application/javascript"
            bodyEquals "console.log(\"Hello World\");"
