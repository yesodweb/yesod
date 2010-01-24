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
    ( Response (..)
      -- * Abnormal responses
    , ErrorResult (..)
    , getHeaders
    , getStatus
      -- * Header
    , Header (..)
    , toPair
      -- * Converting to Hack values
    , responseToHackResponse
#if TEST
      -- * Tests
    , testSuite
#endif
    ) where

#if TEST
import Yesod.Rep hiding (testSuite)
#else
import Yesod.Rep
#endif

import Data.Time.Clock

import Web.Encodings (formatW3)
import qualified Hack

#if TEST
import Test.Framework (testGroup, Test)
#endif

import Data.Generics
import Control.Exception (Exception)
import Data.Convertible.Text (cs)
import Web.Mime

data Response = Response Int [Header] ContentType Content
    deriving Show

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
    deriving (Eq, Show)

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

responseToHackResponse :: [String] -- ^ language list
                       -> Response -> IO Hack.Response
responseToHackResponse _FIXMEls (Response sc hs ct c) = do
    hs' <- mapM toPair hs
    let hs'' = ("Content-Type", cs ct) : hs'
    let asLBS = unContent c
    return $ Hack.Response sc hs'' asLBS

#if TEST
----- Testing
testSuite :: Test
testSuite = testGroup "Yesod.Response"
    [
    ]
#endif
