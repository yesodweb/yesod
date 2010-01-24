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
    , getParam
    , postParam
    , parseEnv
    , runRequest
    , cookies
    , getParams
    , postParams
      -- * Building actual request
    , Request (..)
    , Hack.RequestMethod (..)
#if TEST
    , testSuite
#endif
    ) where

import qualified Hack
import Data.Function.Predicate (equals)
import Yesod.Parameter
import Control.Applicative (Applicative (..))
import Web.Encodings
import qualified Data.ByteString.Lazy as BL
import Data.Convertible.Text
import Control.Arrow ((***))
import Control.Exception (SomeException (..))
import Data.Attempt
import Data.Maybe (fromMaybe)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

newtype Request v = Request { unRequest :: RawRequest
                                        -> Either ParamException v }
instance Functor Request where
    fmap f (Request r) = Request $ fmap f . r
instance Applicative Request where
    pure = Request . const . Right
    (Request f) <*> (Request r) = Request helper where
        helper rr = helper2 (f rr) (r rr)
        helper2 (Left e1) (Left e2) = Left $ e1 ++ e2
        helper2 (Left e) _ = Left e
        helper2 _ (Left e) = Left e
        helper2 (Right f') (Right r') = Right $ f' r'

class RequestReader m where
    getRawRequest :: m RawRequest
    invalidParams :: ParamException -> m a
instance RequestReader Request where
    getRawRequest = Request $ Right
    invalidParams = Request . const . Left

runRequest :: (Monad m, RequestReader m) => Request a -> m a
runRequest (Request f) = do
    rr <- getRawRequest
    either invalidParams return $ f rr

-- | Helper function for generating 'RequestParser's from various
-- 'ParamValue' lists.
genParam :: Parameter a
         => (RawRequest -> ParamName -> [ParamValue])
         -> ParamType
         -> ParamName
         -> Request a
genParam f ptype name = Request helper where
  helper req = attempt failureH Right $ readParams pvs where
      pvs = f req name
      failureH e = Left [((ptype, name, pvs), SomeException e)]

-- | Parse a value passed as a GET parameter.
getParam :: (Parameter a) => ParamName -> Request a
getParam = genParam getParams GetParam

-- | Parse a value passed as a POST parameter.
postParam :: (Parameter a) => ParamName -> Request a
postParam = genParam postParams PostParam

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
     in RawRequest gets' cookies' posts files env

#if TEST
testSuite :: Test
testSuite = testGroup "Yesod.Request"
    [ testCase "Request applicative instance" caseAppInst
    ]

caseAppInst :: Assertion
caseAppInst = do
    let r5 = Request $ const $ Right (5 :: Int)
        rAdd2 = Request $ const $ Right (+ 2)
        r7 = Request $ const $ Right (7 :: Int)
        rr = undefined
        myEquals e t = (unRequest e) rr `myEquals2` (unRequest t) rr
        myEquals2 x y = show x @=? show y
    r5 `myEquals` pure (5 :: Int)
    r7 `myEquals` (rAdd2 <*> r5)
#endif
