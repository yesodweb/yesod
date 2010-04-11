{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
      -- * Request
      Request (..)
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
import Data.Function.Predicate (equals)
import Yesod.Definitions
import Web.Encodings
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Convertible.Text
import Control.Arrow ((***))
import Data.Maybe (fromMaybe)
import "transformers" Control.Monad.IO.Class
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
    getRequest :: m Request
instance RequestReader ((->) Request) where
    getRequest = id

languages :: (Functor m, RequestReader m) => m [Language]
languages = reqLangs `fmap` getRequest

-- | Get the req 'W.Request' value.
waiRequest :: (Functor m, RequestReader m) => m W.Request
waiRequest = reqWaiRequest `fmap` getRequest

type RequestBodyContents =
    ( [(ParamName, ParamValue)]
    , [(ParamName, FileInfo String BL.ByteString)]
    )

-- | The req information passed through W, cleaned up a bit.
data Request = Request
    { reqGetParams :: [(ParamName, ParamValue)]
    , reqCookies :: [(ParamName, ParamValue)]
    , reqSession :: [(B.ByteString, B.ByteString)]
    , reqRequestBody :: IO RequestBodyContents
    , reqWaiRequest :: W.Request
    , reqLangs :: [Language]
    }

multiLookup :: [(ParamName, ParamValue)] -> ParamName -> [ParamValue]
multiLookup [] _ = []
multiLookup ((k, v):rest) pn
    | k == pn = v : multiLookup rest pn
    | otherwise = multiLookup rest pn

-- | All GET paramater values with the given name.
getParams :: Request -> ParamName -> [ParamValue]
getParams rr = multiLookup $ reqGetParams rr

-- | All POST paramater values with the given name.
postParams :: MonadIO m => Request -> m (ParamName -> [ParamValue])
postParams rr = do
    (pp, _) <- liftIO $ reqRequestBody rr
    return $ multiLookup pp

-- | Produces a \"compute on demand\" value. The computation will be run once
-- it is requested, and then the result will be stored. This will happen only
-- once.
iothunk :: IO a -> IO (IO a)
iothunk = fmap go . newMVar . Left where
    go :: MVar (Either (IO a) a) -> IO a
    go mvar = modifyMVar mvar go'
    go' :: Either (IO a) a -> IO (Either (IO a) a, a)
    go' (Right val) = return (Right val, val)
    go' (Left comp) = do
        val <- comp
        return (Right val, val)

-- | All cookies with the given name.
cookies :: Request -> ParamName -> [ParamValue]
cookies rr name = map snd . filter (fst `equals` name) . reqCookies $ rr

parseWaiRequest :: W.Request
                -> [(B.ByteString, B.ByteString)] -- ^ session
                -> IO Request
parseWaiRequest env session = do
    let gets' = map (cs *** cs) $ decodeUrlPairs $ W.queryString env
    let reqCookie = fromMaybe B.empty $ lookup W.Cookie $ W.requestHeaders env
        cookies' = map (cs *** cs) $ parseCookies reqCookie
        acceptLang = lookup W.AcceptLanguage $ W.requestHeaders env
        langs = map cs $ maybe [] parseHttpAccept acceptLang
        langs' = case lookup langKey cookies' of
                    Nothing -> langs
                    Just x -> x : langs
        langs'' = case lookup langKey gets' of
                     Nothing -> langs'
                     Just x -> x : langs'
    rbthunk <- iothunk $ rbHelper env
    return $ Request gets' cookies' session rbthunk env langs''

rbHelper :: W.Request -> IO RequestBodyContents
rbHelper = fmap (fix1 *** map fix2) . parseRequestBody lbsSink where
    fix1 = map (cs *** cs)
    fix2 (x, FileInfo a b c) = (cs x, FileInfo (cs a) (cs b) c)

#if TEST
testSuite :: Test
testSuite = testGroup "Yesod.Request"
    [
    ]
#endif
