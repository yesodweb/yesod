{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses #-}
import Yesod.Core
import Yesod.Json
import Test.Hspec
import qualified Data.Map as Map
import Network.Wai.Test
import Data.Text (Text)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler RepPlain
getHomeR = do
    val <- parseJsonBody_
    case Map.lookup ("foo" :: Text) val of
        Nothing -> invalidArgs ["foo not found"]
        Just foo -> return $ RepPlain $ toContent (foo :: Text)

main :: IO ()
main = do
    app <- toWaiApp App
    hspec $ describe "Yesod.Json" $ do
        it "parses valid content" $ flip runSession app $ do
            sres <- srequest SRequest
                { simpleRequest = defaultRequest
                , simpleRequestBody = "{\"foo\":\"bar\"}"
                }
            assertStatus 200 sres
            assertBody "bar" sres
        it "400 for bad JSON" $ flip runSession app $ do
            sres <- srequest SRequest
                { simpleRequest = defaultRequest
                , simpleRequestBody = "{\"foo\":\"bar\""
                }
            assertStatus 400 sres
        it "400 for bad structure" $ flip runSession app $ do
            sres <- srequest SRequest
                { simpleRequest = defaultRequest
                , simpleRequestBody = "{\"foo2\":\"bar\"}"
                }
            assertStatus 400 sres
            assertBodyContains "foo not found" sres
