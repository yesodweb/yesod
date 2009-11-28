{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-} -- FIXME remove
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
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
    ( Response (..)
      -- * Representations
    , RepT
    , chooseRep
    , HasReps (..)
    , ContentType
      -- * Content
    , Content
    , ToContent (..)
    , runContent
    , translateContent
      -- * Abnormal responses
    , ErrorResult (..)
    , getHeaders
    , getStatus
      -- * Header
    , Header (..)
    , toPair
      -- * Generic responses
    , genResponse
    , htmlResponse
    , objectResponse
#if TEST
      -- * Tests
    , testSuite
#endif
    ) where

import Data.Time.Clock
import Data.Object
import Data.Object.Text
import Data.Object.Translate
import Data.Object.Instances
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

import Web.Encodings (formatW3)

#if TEST
import Test.Framework (testGroup, Test)
#endif

import Data.Generics
import Control.Exception (Exception)
import Data.Maybe (fromJust)
import Data.Convertible

import Data.Text.Lazy (Text)

data Response = Response Int [Header] ContentType Content

type ContentType = String

data Content = ByteString LBS.ByteString
             | Text LT.Text
             | TransText ([Language] -> LT.Text)

runContent :: [Language] -> Content -> LBS.ByteString
runContent _ (ByteString lbs) = lbs
runContent _ (Text lt) = LTE.encodeUtf8 lt
runContent ls (TransText t) = LTE.encodeUtf8 $ t ls

class ToContent a where
    toContent :: a -> Content
instance ToContent SBS.ByteString where
    toContent = ByteString . convertSuccess
instance ToContent LBS.ByteString where
    toContent = ByteString
instance ToContent String where
    toContent = Text . LT.pack
instance ToContent Text where
    toContent = Text
instance ToContent ([Language] -> String) where
    toContent f = TransText $ LT.pack . f
instance ToContent Translator where
    toContent = TransText

translateContent :: CanTranslate t => t -> Content
translateContent t = toContent $ translate t

type RepT m = (ContentType, m Content)

chooseRep :: Monad m
          => [ContentType]
          -> [RepT m]
          -> RepT m
chooseRep cs rs
  | null rs = error "All reps must have at least one representation" -- FIXME
  | otherwise = do
    let availCs = map fst rs
    case filter (`elem` availCs) cs of
        [] -> head rs
        [ctype] -> (ctype, fromJust $ lookup ctype rs) -- FIXME
        _ -> error "Overlapping representations" -- FIXME just take the first?

-- | Something which can be represented as multiple content types.
-- Each content type is called a representation of the data.
class Monad m => HasReps a m where
    -- | Provide an ordered list of possible representations, depending on
    -- content type. If the user asked for a specific response type (like
    -- text/html), then that will get priority. If not, then the first
    -- element in this list will be used.
    reps :: a -> [RepT m]

-- | Abnormal return codes.
data ErrorResult =
    Redirect String
    | NotFound
    | InternalError String
    | InvalidArgs [(String, String)]
    | PermissionDenied
    deriving (Show, Typeable)
instance Exception ErrorResult

getStatus :: ErrorResult -> Int
getStatus (Redirect _) = 303
getStatus NotFound = 404
getStatus (InternalError _) = 500
getStatus (InvalidArgs _) = 400
getStatus PermissionDenied = 403

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
-- FIXME move these to Handler?
-- | Return a response with an arbitrary content type.
genResponse :: (Monad m, ConvertSuccess t Text)
            => ContentType
            -> t
            -> [RepT m]
genResponse ct t = [(ct, return $ Text $ convertSuccess t)]

-- | Return a response with a text/html content type.
htmlResponse :: (Monad m, ConvertSuccess t Text) => t -> [RepT m]
htmlResponse = genResponse "text/html"

-- | Return a response from an Object.
objectResponse :: (Monad m, ToObject o Text Text) => o -> [RepT m]
objectResponse = reps . toTextObject

-- HasReps instances
instance Monad m => HasReps () m where
    reps _ = [("text/plain", return $ toContent "")]
instance Monad m => HasReps TextObject m where
    reps o =
        [ ("text/html", return $ toContent $ unHtml $ convertSuccess o)
        , ("application/json", return $ toContent $ unJson $ convertSuccess o)
        , ("text/yaml", return $ toContent $ unYaml $ convertSuccess o)
        ]

{- FIXME
instance HasReps (Reps m) where
    reps = id
-}

#if TEST
----- Testing
testSuite :: Test
testSuite = testGroup "Web.Restful.Response"
    [
    ]
#endif
