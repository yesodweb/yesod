{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
---------------------------------------------------------
--
-- Module        : Yesod.Response
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
module Yesod.Response
    ( -- * Representations
      Content (..)
    , ChooseRep
    , HasReps (..)
    , defChooseRep
    , Response (..)
      -- * Special responses
    , RedirectType (..)
    , getRedirectStatus
    , SpecialResponse (..)
      -- * Error responses
    , ErrorResponse (..)
    , getStatus
      -- * Header
    , Header (..)
    , headerToPair
      -- * Converting to Hack values
    , responseToHackResponse
#if TEST
      -- * Tests
    , testSuite
#endif
    ) where

import Data.Time.Clock
import Data.Maybe (mapMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Yesod.Definitions

import Web.Encodings (formatW3)
import qualified Hack

#if TEST
import Data.Object.Html hiding (testSuite)
#else
import Data.Object.Html
#endif

#if TEST
import Test.Framework (testGroup, Test)
#endif

import Data.Convertible.Text (cs)
import Web.Mime

newtype Content = Content { unContent :: [Language] -> IO ByteString }

instance ConvertSuccess Text Content where
    convertSuccess = Content . const . return . cs
instance ConvertSuccess ByteString Content where
    convertSuccess = Content . const . return
instance ConvertSuccess String Content where
    convertSuccess = Content . const . return . cs
instance ConvertSuccess HtmlDoc Content where
    convertSuccess = cs . unHtmlDoc
instance ConvertSuccess XmlDoc Content where
    convertSuccess = cs . unXmlDoc

type ChooseRep = [ContentType] -> IO (ContentType, Content)

-- | Any type which can be converted to representations. There must be at least
-- one representation for each type.
class HasReps a where
    chooseRep :: a -> ChooseRep

-- | A helper method for generating 'HasReps' instances.
defChooseRep :: [(ContentType, a -> IO Content)] -> a -> ChooseRep
defChooseRep reps a ts = do
  let (ct, c) =
        case mapMaybe helper ts of
            (x:_) -> x
            [] -> case reps of
                    [] -> error "Empty reps"
                    (x:_) -> x
  c' <- c a
  return (ct, c')
        where
            helper ct = do
                c <- lookup ct reps
                return (ct, c)

instance HasReps ChooseRep where
    chooseRep = id

instance HasReps () where
    chooseRep = defChooseRep [(TypePlain, const $ return $ cs "")]

instance HasReps [(ContentType, Content)] where
    chooseRep a cts = return $
        case filter (\(ct, _) -> ct `elem` cts) a of
            ((ct, c):_) -> (ct, c)
            _ -> case a of
                    (x:_) -> x
                    _ -> error "chooseRep [(ContentType, Content)] of empty"

data Response = Response Int [Header] ContentType Content

-- | Different types of redirects.
data RedirectType = RedirectPermanent
                  | RedirectTemporary
                  | RedirectSeeOther
    deriving (Show, Eq)

getRedirectStatus :: RedirectType -> Int
getRedirectStatus RedirectPermanent = 301
getRedirectStatus RedirectTemporary = 302
getRedirectStatus RedirectSeeOther = 303

-- | Special types of responses which should short-circuit normal response
-- processing.
data SpecialResponse =
      Redirect RedirectType String
    | SendFile ContentType FilePath
    deriving (Show, Eq)

-- | Responses to indicate some form of an error occurred. These are different
-- from 'SpecialResponse' in that they allow for custom error pages.
data ErrorResponse =
      NotFound
    | InternalError String
    | InvalidArgs [(String, String)]
    | PermissionDenied
    deriving (Show, Eq)

getStatus :: ErrorResponse -> Int
getStatus NotFound = 404
getStatus (InternalError _) = 500
getStatus (InvalidArgs _) = 400
getStatus PermissionDenied = 403

----- header stuff
-- | Headers to be added to a 'Result'.
data Header =
    AddCookie Int String String
    | DeleteCookie String
    | Header String String
    deriving (Eq, Show)

-- | Convert Header to a key/value pair.
headerToPair :: Header -> IO (String, String)
headerToPair (AddCookie minutes key value) = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral $ minutes * 60) now
    return ("Set-Cookie", key ++ "=" ++ value ++"; path=/; expires="
                              ++ formatW3 expires)
headerToPair (DeleteCookie key) = return
    ("Set-Cookie",
     key ++ "=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT")
headerToPair (Header key value) = return (key, value)

responseToHackResponse :: [String] -- ^ language list
                       -> Response -> IO Hack.Response
responseToHackResponse ls (Response sc hs ct c) = do
    hs' <- mapM headerToPair hs
    let hs'' = ("Content-Type", cs ct) : hs'
    asLBS <- unContent c ls
    return $ Hack.Response sc hs'' asLBS

#if TEST
----- Testing
testSuite :: Test
testSuite = testGroup "Yesod.Response"
    [
    ]
#endif
