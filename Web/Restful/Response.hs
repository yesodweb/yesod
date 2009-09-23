{-# LANGUAGE FlexibleInstances #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Response
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Generating responses.
--
---------------------------------------------------------
module Web.Restful.Response
    ( -- * Representations
      Reps
    , HasReps (..)
    , ContentType
      -- * Abnormal responses
    , ErrorResult (..)
    , getHeaders
    , getStatus
      -- * Header
    , Header (..)
    , toPair
      -- * Generic responses
    , response
    , genResponse
    , htmlResponse
    , objectResponse
      -- * Tests
    , testSuite
    ) where

import Data.ByteString.Class
import Data.Time.Clock
import Data.Object
import qualified Data.ByteString.Lazy as B
import Data.Object.Instances

import Web.Restful.Utils (formatW3)

import Test.Framework (testGroup, Test)

type ContentType = String

type Reps = [(ContentType, B.ByteString)]

-- | Something which can be represented as multiple content types.
-- Each content type is called a representation of the data.
class HasReps a where
    -- | Provide an ordered list of possible representations, depending on
    -- content type. If the user asked for a specific response type (like
    -- text/html), then that will get priority. If not, then the first
    -- element in this list will be used.
    reps :: a -> Reps

-- | Abnormal return codes.
data ErrorResult =
    Redirect String
    | NotFound
    | InternalError String
    | InvalidArgs [(String, String)]

getStatus :: ErrorResult -> Int
getStatus (Redirect _) = 303
getStatus NotFound = 404
getStatus (InternalError _) = 500
getStatus (InvalidArgs _) = 400

getHeaders :: ErrorResult -> [Header]
getHeaders (Redirect s) = [Header "Location" s]
getHeaders _ = []

----- header stuff
-- | Headers to be added to a 'Result'.
data Header =
    AddCookie Int String String
    | DeleteCookie String
    | Header String String

-- | Convert Header to a key/value pair.
toPair :: Header -> IO (String, String)
toPair (AddCookie minutes key value) = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral $ minutes * 60) now
    return ("Set-Cookie", key ++ "=" ++ value ++"; path=/; expires="
                              ++ formatW3 expires)
toPair (DeleteCookie key) = return
    ("Set-Cookie",
     key ++ "=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT")
toPair (Header key value) = return (key, value)

------ Generic responses
-- | Lifts a 'HasReps' into a monad.
response :: (Monad m, HasReps reps) => reps -> m Reps
response = return . reps

-- | Return a response with an arbitrary content type.
genResponse :: (Monad m, LazyByteString lbs)
            => ContentType
            -> lbs
            -> m Reps
genResponse ct lbs = return [(ct, toLazyByteString lbs)]

-- | Return a response with a text/html content type.
htmlResponse :: (Monad m, LazyByteString lbs) => lbs -> m Reps
htmlResponse = genResponse "text/html"

-- | Return a response from an Object.
objectResponse :: (Monad m, ToObject o) => o -> m Reps
objectResponse = return . reps . toObject

-- HasReps instances
instance HasReps () where
    reps _ = [("text/plain", toLazyByteString "")]
instance HasReps Object where
    reps o =
        [ ("text/html", unHtml $ safeFromObject o)
        , ("application/json", unJson $ safeFromObject o)
        , ("text/yaml", unYaml $ safeFromObject o)
        ]

instance HasReps [(ContentType, B.ByteString)] where
    reps = id

----- Testing
testSuite :: Test
testSuite = testGroup "Web.Restful.Response"
    [
    ]
