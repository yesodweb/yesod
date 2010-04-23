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
import Yesod.Definitions
import Control.Monad (when)
import Yesod.Handler
import Yesod.Content

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Data.Text.Lazy (unpack)
import qualified Data.Text as T
#endif

newtype Json url a = Json { unJson :: Hamlet url IO a }
    deriving (Functor, Applicative, Monad)

jsonToContent :: Json (Routes master) () -> GHandler sub master Content
jsonToContent = hamletToContent . unJson

htmlContentToText :: HtmlContent -> Text
htmlContentToText (Encoded t) = t
htmlContentToText (Unencoded t) = encodeHtml t

jsonScalar :: HtmlContent -> Json url ()
jsonScalar s = Json $ do
    outputString "\""
    output $ encodeJson $ htmlContentToText s
    outputString "\""

jsonList :: [Json url ()] -> Json url ()
jsonList = jsonList' . fromList

jsonList' :: Enumerator (Json url ()) (Json url) -> Json url () -- FIXME simplify type
jsonList' (Enumerator enum) = do
    Json $ outputString "["
    _ <- enum go False
    Json $ outputString "]"
  where
    go putComma j = do
        when putComma $ Json $ outputString ","
        () <- j
        return $ Right True

jsonMap :: [(Json url (), Json url ())] -> Json url ()
jsonMap = jsonMap' . fromList

jsonMap' :: Enumerator (Json url (), Json url ()) (Json url) -> Json url () -- FIXME simplify type
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
