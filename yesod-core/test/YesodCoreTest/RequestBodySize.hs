{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.RequestBodySize
    ( specs
    , Widget
    , resourcesY
    ) where

import Test.Hspec

import Yesod.Core

import Network.Wai
import Network.Wai.Test
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Conduit
import Data.Conduit.List (consume)
import Data.Conduit.Binary (isolate)

data Y = Y

mkYesod "Y" [parseRoutes|
/post PostR POST
/consume ConsumeR POST
/partial-consume PartialConsumeR POST
/unused UnusedR POST
|]

instance Yesod Y where
    maximumContentLength _ _ = Just 10

postPostR, postConsumeR, postPartialConsumeR, postUnusedR :: Handler RepPlain

postPostR = do
    val <- lookupPostParams "foobarbaz"
    return $ RepPlain $ toContent $ T.concat val

postConsumeR = do
    body <- rawRequestBody $$ consume
    return $ RepPlain $ toContent $ S.concat body

postPartialConsumeR = do
    body <- rawRequestBody $$ isolate 5 =$ consume
    return $ RepPlain $ toContent $ S.concat body

postUnusedR = return $ RepPlain ""

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

caseHelper :: String -- ^ name
           -> Text -- ^ pathinfo
           -> ByteString -- ^ request body
           -> Int -- ^ expected status code, chunked
           -> Int -- ^ expected status code, non-chunked
           -> Spec
caseHelper name path body statusChunked statusNonChunked = describe name $ do
    it "chunked" $ runner $ do
        res <- mkRequest False
        assertStatus statusChunked res
    it "non-chunked" $ runner $ do
        res <- mkRequest True
        assertStatus statusNonChunked res
  where
    mkRequest includeLength = srequest $ SRequest defaultRequest
        { pathInfo = [path]
        , requestHeaders =
            ("content-type", "application/x-www-form-urlencoded") :

            if includeLength
                then [("content-length", S8.pack $ show $ S.length body)]
                else []
        , requestMethod = "POST"
        , requestBodyLength =
            if includeLength
                then KnownLength $ fromIntegral $ S.length body
                else ChunkedBody
        } $ L.fromChunks $ map S.singleton $ S.unpack body

specs :: Spec
specs = describe "Test.RequestBodySize" $ do
    caseHelper "lookupPostParam- large" "post" "foobarbaz=bin" 413 413
    caseHelper "lookupPostParam- small" "post" "foo=bin" 200 200
    caseHelper "total consume- large" "consume" "this is longer than 10" 413 413
    caseHelper "total consume- small" "consume" "smaller" 200 200
    caseHelper "partial consume- large" "partial-consume" "this is longer than 10" 200 413
    caseHelper "partial consume- small" "partial-consume" "smaller" 200 200
    caseHelper "unused- large" "unused" "this is longer than 10" 200 413
    caseHelper "unused- small" "unused" "smaller" 200 200
