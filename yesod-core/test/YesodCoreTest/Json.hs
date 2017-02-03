{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ViewPatterns #-}
module YesodCoreTest.Json
    ( specs
    , Widget
    , resourcesApp
    ) where

import Yesod.Core
import Test.Hspec
import qualified Data.Map as Map
import Network.Wai.Test
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/has-multiple-pieces/#Int/#Int MultiplePiecesR GET
|]

instance Yesod App

getHomeR :: Handler RepPlain
getHomeR = do
    val <- requireJsonBody
    case Map.lookup ("foo" :: Text) val of
        Nothing -> invalidArgs ["foo not found"]
        Just foo -> return $ RepPlain $ toContent (foo :: Text)

getMultiplePiecesR :: Int -> Int -> Handler ()
getMultiplePiecesR _ _ = return ()

test :: String
     -> ByteString
     -> (SResponse -> Session ())
     -> Spec
test name rbody f = it name $ do
    app <- toWaiApp App
    flip runSession app $ do
        sres <- srequest SRequest
            { simpleRequest = defaultRequest
            , simpleRequestBody = rbody
            }
        f sres

specs :: Spec
specs = describe "Yesod.Json" $ do
    test "parses valid content" "{\"foo\":\"bar\"}" $ \sres -> do
        assertStatus 200 sres
        assertBody "bar" sres
    test "400 for bad JSON" "{\"foo\":\"bar\"" $ \sres -> do
        assertStatus 400 sres
    test "400 for bad structure" "{\"foo2\":\"bar\"}" $ \sres -> do
        assertStatus 400 sres
        assertBodyContains "foo not found" sres
