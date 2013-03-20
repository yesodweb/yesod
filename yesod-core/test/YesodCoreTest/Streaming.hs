{-# LANGUAGE OverloadedStrings #-}
module YesodCoreTest.Streaming (specs) where

import Yesod.Core
import Test.Hspec
import Network.Wai.Test
import Data.Conduit
import Blaze.ByteString.Builder (fromByteString)

app :: LiteApp
app = dispatchTo $ respondSource typeHtml $
    yield $ Chunk $ fromByteString "Hello World!"

test :: String
     -> (SResponse -> Session ())
     -> Spec
test name f = it name $ do
    wapp <- toWaiApp app
    flip runSession wapp $ do
        sres <- request defaultRequest
        f sres

specs :: Spec
specs = describe "Streaming" $ do
    test "works" $ \sres -> do
        assertStatus 200 sres
        assertBody "Hello World!" sres
