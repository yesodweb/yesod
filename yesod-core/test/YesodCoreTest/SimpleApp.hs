module YesodCoreTest.SimpleApp (specs) where

import Yesod.Core
import Test.Hspec
import Network.Wai.Test
import Network.Wai
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as L8

iapp :: IO Application
iapp = toWaiApp $
    onMethod (T.pack "GET") (serveHandler $ return "GetHomepage") <>
    onMethod (T.pack "POST") (serveHandler $ return "PostHomepage") <>
    onStatic (T.pack "string") (withDynamic (\t -> serveHandler $ return (t :: T.Text)))

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
specs = describe "SimpleApp" $ do
    test "GET" [] $ Right "GetHomepage"
    test "POST" [] $ Right "PostHomepage"
    test "PUT" [] $ Left 405
    test "GET" ["string", "foo"] $ Right "foo"
    test "GET" ["string!", "foo"] $ Left 404
