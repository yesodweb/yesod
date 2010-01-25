{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
---------------------------------------------------------
--
-- Module        : Yesod.Request
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Code for extracting parameters from requests.
--
---------------------------------------------------------
module Yesod.Request
    (
      -- * RawRequest
      RawRequest (..)
    , RequestReader (..)
    , parseEnv
    , cookies
    , getParams
    , postParams
    , languages
      -- * Building actual request
    , Hack.RequestMethod (..)
      -- * Parameter
    , ParamType (..)
    , ParamName
    , ParamValue
    , ParamException
#if TEST
    , testSuite
#endif
    ) where

import qualified Hack
import Data.Function.Predicate (equals)
import Yesod.Definitions
import Web.Encodings
import qualified Data.ByteString.Lazy as BL
import Data.Convertible.Text
import Control.Arrow ((***))
import Control.Exception (SomeException (..))
import Data.Maybe (fromMaybe)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

data ParamType = GetParam | PostParam
type ParamName = String
type ParamValue = String
type ParamException = [((ParamType, ParamName, [ParamValue]), SomeException)]

class RequestReader m where
    getRawRequest :: m RawRequest
    invalidParams :: ParamException -> m a

languages :: (Functor m, RequestReader m) => m [Language]
languages = rawLangs `fmap` getRawRequest

-- | Get the raw 'Hack.Env' value.
parseEnv :: (Functor m, RequestReader m) => m Hack.Env
parseEnv = rawEnv `fmap` getRawRequest

-- | The raw information passed through Hack, cleaned up a bit.
data RawRequest = RawRequest
    { rawGetParams :: [(ParamName, ParamValue)]
    , rawCookies :: [(ParamName, ParamValue)]
    -- FIXME when we switch to WAI, the following two should be combined and
    -- wrapped in the IO monad
    , rawPostParams :: [(ParamName, ParamValue)]
    , rawFiles :: [(ParamName, FileInfo String BL.ByteString)]
    , rawEnv :: Hack.Env
    , rawLangs :: [Language]
    }
    deriving Show

-- | All GET paramater values with the given name.
getParams :: RawRequest -> ParamName -> [ParamValue]
getParams rr name = map snd
                  . filter (\x -> name == fst x)
                  . rawGetParams
                  $ rr

-- | All POST paramater values with the given name.
postParams :: RawRequest -> ParamName -> [ParamValue]
postParams rr name = map snd
                   . filter (\x -> name == fst x)
                   . rawPostParams
                   $ rr

-- | All cookies with the given name.
cookies :: RawRequest -> ParamName -> [ParamValue]
cookies rr name = map snd . filter (fst `equals` name) . rawCookies $ rr

instance ConvertSuccess Hack.Env RawRequest where
  convertSuccess env =
    let gets' = decodeUrlPairs $ Hack.queryString env :: [(String, String)]
        clength = fromMaybe "0" $ lookup "Content-Length" $ Hack.http env
        ctype = fromMaybe "" $ lookup "Content-Type" $ Hack.http env
        convertFileInfo (FileInfo a b c) = FileInfo (cs a) (cs b) c
        (posts, files) = map (convertSuccess *** convertSuccess) ***
                         map (convertSuccess *** convertFileInfo)
                       $ parsePost ctype clength
                       $ Hack.hackInput env
        rawCookie = fromMaybe "" $ lookup "Cookie" $ Hack.http env
        cookies' = decodeCookies rawCookie :: [(String, String)]
        acceptLang = lookup "Accept-Language" $ Hack.http env
        langs = maybe [] parseHttpAccept acceptLang
        langs' = case lookup langKey cookies' of
                    Nothing -> langs
                    Just x -> x : langs
        langs'' = case lookup langKey gets' of
                     Nothing -> langs'
                     Just x -> x : langs'
     in RawRequest gets' cookies' posts files env langs''

#if TEST
testSuite :: Test
testSuite = testGroup "Yesod.Request"
    [
    ]
#endif
