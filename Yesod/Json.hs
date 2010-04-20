{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Yesod.Json
    ( Json
    , jsonToContent
      -- * Generate Json output
    , jsonScalar
    , jsonList
    , jsonList'
    , jsonMap
    , jsonMap'
#if TEST
    , testSuite
#endif
    )
    where

import Text.Hamlet.Monad
import Control.Applicative
import Data.Text (Text)
import Web.Encodings
import Yesod.Hamlet
import Control.Monad (when)
#if TEST
import Yesod.Response hiding (testSuite)
import Data.Text.Lazy (unpack)
import qualified Data.Text as T
#else
import Yesod.Response
#endif
import Yesod.Handler

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Control.Monad (when)
#endif

newtype Json url m a = Json { unJson :: Hamlet url m a }
    deriving (Functor, Applicative, Monad)

jsonToContent :: Json (Routes sub) IO () -> GHandler sub master Content
jsonToContent = hamletToContent . unJson

htmlContentToText :: HtmlContent -> Text
htmlContentToText (Encoded t) = t
htmlContentToText (Unencoded t) = encodeHtml t

jsonScalar :: Monad m => HtmlContent -> Json url m ()
jsonScalar s = Json $ do
    outputString "\""
    output $ encodeJson $ htmlContentToText s
    outputString "\""

jsonList :: Monad m => [Json url m ()] -> Json url m ()
jsonList = jsonList' . fromList

jsonList' :: Monad m => Enumerator (Json url m ()) (Json url m) -> Json url m () -- FIXME simplify type
jsonList' (Enumerator enum) = do
    Json $ outputString "["
    _ <- enum go False
    Json $ outputString "]"
  where
    go putComma j = do
        when putComma $ Json $ outputString ","
        () <- j
        return $ Right True

jsonMap :: Monad m => [(Json url m (), Json url m ())] -> Json url m ()
jsonMap = jsonMap' . fromList

jsonMap' :: Monad m => Enumerator (Json url m (), Json url m ()) (Json url m) -> Json url m () -- FIXME simplify type
jsonMap' (Enumerator enum) = do
    Json $ outputString "{"
    _ <- enum go False
    Json $ outputString "}"
  where
    go putComma (k, v) = do
        when putComma $ Json $ outputString ","
        () <- k
        Json $ outputString ":"
        () <- v
        return $ Right True

#if TEST

testSuite :: Test
testSuite = testGroup "Yesod.Json"
    [ testCase "simple output" caseSimpleOutput
    ]

caseSimpleOutput :: Assertion
caseSimpleOutput = do
    let j = do
        jsonMap
            [ (jsonScalar $ T.pack "foo" , jsonList
                [ jsonScalar $ T.pack "bar"
                , jsonScalar $ T.pack "baz"
                ])
            ]
    t <- hamletToText id $ unJson j
    "{\"foo\":[\"bar\",\"baz\"]}" @=? unpack t

#endif
