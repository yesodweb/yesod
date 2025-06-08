{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module YesodCoreTest.LiteApp (specs) where

import Yesod.Core
import Test.Hspec
import Network.Wai.Test
import Network.Wai
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

iapp :: IO Application
iapp = toWaiApp $ liteApp $ do
    onMethod (S8.pack "GET") (dispatchTo $ return "GetHomepage")
    onMethod (S8.pack "POST") (dispatchTo $ return "PostHomepage")
    onStatic (T.pack "string") (withDynamic (\t -> dispatchTo $ return (t :: T.Text)))
    onStatic (T.pack "multi") (withDynamicMulti (\[_, y] -> dispatchTo $ return (y :: T.Text)))

test :: String -- ^ method
     -> [String] -- ^ path
     -> (Either Int String) -- ^ status code or body
     -> Spec
test method path expected = it (method ++ " " ++ show path) $ do
    app <- iapp
    flip runSession app $ do
        sres <- request defaultRequest
            { requestMethod = S8.pack method
            , pathInfo = map T.pack path
            }
        case expected of
            Left i -> assertStatus i sres
            Right b -> assertBody (L8.pack b) sres

specs :: Spec
specs = describe "LiteApp" $ do
    test "GET" [] $ Right "GetHomepage"
    test "POST" [] $ Right "PostHomepage"
    -- test "PUT" [] $ Left 405
    test "GET" ["string", "foo"] $ Right "foo"
    test "DELETE" ["string", "bar"] $ Right "bar"
    test "GET" ["string!", "foo"] $ Left 404
    test "GET" ["multi", "foo", "bar"] $ Right "bar"
    test "GET" ["multi", "foo", "bar", "baz"] $ Left 500
