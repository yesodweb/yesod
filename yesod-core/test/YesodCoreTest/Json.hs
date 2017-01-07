{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ViewPatterns #-}
module YesodCoreTest.Json (specs, Widget) where

import Yesod.Core
import Test.Hspec
import qualified Data.Map as Map
import Network.Wai.Test
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Network.Wai ( Request(..) )
import Data.Maybe (fromMaybe, isJust)
import Data.Aeson (Result)
import Control.Monad (when, void)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/urlencoded UrlEncodedR POST
/urlencoded/reversed UrlEncodedReversedR POST
/has-multiple-pieces/#Int/#Int MultiplePiecesR GET
|]

instance Yesod App

getHomeR :: Handler RepPlain
getHomeR = do
    val <- requireJsonBody
    case Map.lookup ("foo" :: Text) val of
        Nothing -> invalidArgs ["foo not found"]
        Just foo -> return $ RepPlain $ toContent (foo :: Text)

-- | Demonstrates effect of first checking JSON, then trying to access post params
postUrlEncodedR :: Handler RepPlain
postUrlEncodedR = do
    j <- lookupGetParam "parseJSON"
    when (isJust j) $ void (parseJsonBody :: Handler (Result Text))
    RepPlain . toContent . fromMaybe "" <$> lookupPostParam "test"

-- | Demonstrates effect of first checking post params, then trying to parse as JSON
postUrlEncodedReversedR :: Handler RepPlain
postUrlEncodedReversedR = do
    void $ lookupPostParam "test"
    RepPlain . toContent <$> (requireJsonBody :: Handler Text)

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

data Mode = OnlyPost | Both | BothReversed

testUrlEncoded :: Mode
               -> String
               -> ByteString
               -> (SResponse -> Session ())
               -> Spec
testUrlEncoded m name rbody f = it name $ do
    app <- toWaiApp App
    flip runSession app $ do
        sres <- srequest SRequest
            { simpleRequest = setCtMethod $ setPath defaultRequest reqPath
            , simpleRequestBody = rbody
            }
        f sres
  where
    setCtMethod req = let ct = ("Content-type", "application/x-www-form-urlencoded")
                        in req { requestHeaders = ct : requestHeaders req
                               , requestMethod = "POST"
                               }
    reqPath = case m of OnlyPost     -> "/urlencoded"
                        Both         -> "/urlencoded?parseJSON=yes"
                        BothReversed -> "/urlencoded/reversed"

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
    testUrlEncoded OnlyPost "post parameters are available" "test=val" $ \sres -> do
        assertStatus 200 sres
        assertBody "val" sres
    testUrlEncoded Both "post parameters are available after parsing JSON from body" "test=val" $ \sres -> do
        assertStatus 200 sres
        assertBody "val" sres
    testUrlEncoded BothReversed "JSON in available after querying post parameters" "\"val\"" $ \sres -> do
        assertStatus 200 sres
        assertBody "val" sres
