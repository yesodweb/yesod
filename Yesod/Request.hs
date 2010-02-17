{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
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
    , waiRequest
    , cookies
    , getParams
    , postParams
    , languages
    , parseWaiRequest
      -- * Parameter
    , ParamName
    , ParamValue
    , ParamError
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
import Data.Maybe (fromMaybe)
import "transformers" Control.Monad.Trans
import Control.Concurrent.MVar

#if TEST
import Test.Framework (testGroup, Test)
--import Test.Framework.Providers.HUnit
--import Test.HUnit hiding (Test)
#endif

type ParamName = String
type ParamValue = String
type ParamError = String

class RequestReader m where
    getRawRequest :: m RawRequest
instance RequestReader ((->) RawRequest) where
    getRawRequest = id

languages :: (Functor m, RequestReader m) => m [Language]
languages = rawLangs `fmap` getRawRequest

-- | Get the raw 'W.Request' value.
waiRequest :: (Functor m, RequestReader m) => m W.Request
waiRequest = rawWaiRequest `fmap` getRawRequest

type RequestBodyContents =
    ( [(ParamName, ParamValue)]
    , [(ParamName, FileInfo String BL.ByteString)]
    )

-- | The raw information passed through W, cleaned up a bit.
data RawRequest = RawRequest
    { rawGetParams :: [(ParamName, ParamValue)]
    , rawCookies :: [(ParamName, ParamValue)]
    , rawSession :: [(B.ByteString, B.ByteString)]
    , rawRequestBody :: IO RequestBodyContents
    , rawWaiRequest :: W.Request
    , rawLangs :: [Language]
    }

multiLookup :: [(ParamName, ParamValue)] -> ParamName -> [ParamValue]
multiLookup [] _ = []
multiLookup ((k, v):rest) pn
    | k == pn = v : multiLookup rest pn
    | otherwise = multiLookup rest pn

-- | All GET paramater values with the given name.
getParams :: RawRequest -> ParamName -> [ParamValue]
getParams rr = multiLookup $ rawGetParams rr

-- | All POST paramater values with the given name.
postParams :: MonadIO m => RawRequest -> m (ParamName -> [ParamValue])
postParams rr = do
    (pp, _) <- liftIO $ rawRequestBody rr
    return $ multiLookup pp

-- | All cookies with the given name.
cookies :: RawRequest -> ParamName -> [ParamValue]
cookies rr name = map snd . filter (fst `equals` name) . rawCookies $ rr

parseWaiRequest :: W.Request -> [(B.ByteString, B.ByteString)] -> IO RawRequest
parseWaiRequest env session = do
    let gets' = map (cs *** cs) $ decodeUrlPairs $ W.queryString env
    let rawCookie = fromMaybe B.empty $ lookup W.Cookie $ W.requestHeaders env
        cookies' = map (cs *** cs) $ parseCookies rawCookie
        acceptLang = lookup W.AcceptLanguage $ W.requestHeaders env
        langs = map cs $ maybe [] parseHttpAccept acceptLang
        langs' = case lookup langKey cookies' of
                    Nothing -> langs
                    Just x -> x : langs
        langs'' = case lookup langKey gets' of
                     Nothing -> langs'
                     Just x -> x : langs'
    mrb <- newMVar $ Left env
    return $ RawRequest gets' cookies' session (rbHelper mrb) env langs''

rbHelper :: MVar (Either W.Request RequestBodyContents)
         -> IO RequestBodyContents
rbHelper mvar = modifyMVar mvar helper where
    helper (Right bc) = return (Right bc, bc)
    helper (Left env) = do
        inputLBS <- WE.toLBS $ W.requestBody env -- FIXME
        let clength = maybe "0" cs  $ lookup W.ReqContentLength
                                    $ W.requestHeaders env
        let ctype = maybe "" cs $ lookup W.ReqContentType $ W.requestHeaders env
        let convertFileInfo (FileInfo a b c) = FileInfo (cs a) (cs b) c
        let ret = map (cs *** cs) ***
                  map (cs *** convertFileInfo)
                $ parsePost ctype clength inputLBS
        return (Right ret, ret)

#if TEST
testSuite :: Test
testSuite = testGroup "Yesod.Request"
    [
    ]
#endif
