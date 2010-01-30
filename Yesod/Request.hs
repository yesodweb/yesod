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
    , parseWaiRequest
      -- * Parameter
    , ParamType (..)
    , ParamName
    , ParamValue
    , ParamException
#if TEST
    , testSuite
#endif
    ) where

import qualified Network.Wai as W
import qualified Network.Wai.Enumerator as WE
import Data.Function.Predicate (equals)
import Yesod.Definitions
import Web.Encodings
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Convertible.Text
import Control.Arrow ((***))
import Control.Exception (SomeException (..))
import Data.Maybe (fromMaybe)

#if TEST
import Test.Framework (testGroup, Test)
--import Test.Framework.Providers.HUnit
--import Test.HUnit hiding (Test)
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

-- | Get the raw 'W.Env' value.
parseEnv :: (Functor m, RequestReader m) => m W.Request
parseEnv = rawRequest `fmap` getRawRequest

-- | The raw information passed through W, cleaned up a bit.
data RawRequest = RawRequest
    { rawGetParams :: [(ParamName, ParamValue)]
    , rawCookies :: [(ParamName, ParamValue)]
    , rawSession :: [(B.ByteString, B.ByteString)]
    -- when we switch to WAI, the following two should be combined and
    -- wrapped in the IO monad
    , rawPostParams :: [(ParamName, ParamValue)]
    , rawFiles :: [(ParamName, FileInfo String BL.ByteString)]
    , rawRequest :: W.Request
    , rawLangs :: [Language]
    }

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

parseWaiRequest :: W.Request -> [(B.ByteString, B.ByteString)] -> IO RawRequest
parseWaiRequest env session = do
    let gets' = map (cs *** cs) $ decodeUrlPairs $ W.queryString env
    let clength = maybe "0" cs  $ lookup W.ReqContentLength
                                $ W.httpHeaders env
    let ctype = maybe "" cs $ lookup W.ReqContentType $ W.httpHeaders env
    let convertFileInfo (FileInfo a b c) = FileInfo (cs a) (cs b) c
    inputLBS <- WE.toLBS $ W.requestBody env -- FIXME
    let (posts, files) = map (convertSuccess *** convertSuccess) ***
                         map (convertSuccess *** convertFileInfo)
                       $ parsePost ctype clength
                         inputLBS
        rawCookie = fromMaybe B.empty $ lookup W.Cookie $ W.httpHeaders env
        cookies' = map (cs *** cs) $ decodeCookies rawCookie
        acceptLang = lookup W.AcceptLanguage $ W.httpHeaders env
        langs = map cs $ maybe [] parseHttpAccept acceptLang
        langs' = case lookup langKey cookies' of
                    Nothing -> langs
                    Just x -> x : langs
        langs'' = case lookup langKey gets' of
                     Nothing -> langs'
                     Just x -> x : langs'
    return $ RawRequest gets' cookies' session posts files env langs''

#if TEST
testSuite :: Test
testSuite = testGroup "Yesod.Request"
    [
    ]
#endif
