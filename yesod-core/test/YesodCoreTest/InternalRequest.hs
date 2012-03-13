{-# LANGUAGE OverloadedStrings #-}
module YesodCoreTest.InternalRequest (internalRequestTest) where

import Data.List (nub)
import System.Random (StdGen, mkStdGen)

import Network.Wai as W
import Network.Wai.Test
import Yesod.Internal.TestApi (randomString, parseWaiRequest')
import Yesod.Request (Request (..))
import Test.Hspec

randomStringSpecs :: [Spec]
randomStringSpecs = describe "Yesod.Internal.Request.randomString"
  [ it "looks reasonably random" looksRandom
  , it "does not repeat itself" $ noRepeat 10 100
  ]

-- NOTE: this testcase may break on other systems/architectures if
-- mkStdGen is not identical everywhere (is it?).
looksRandom :: Bool
looksRandom = randomString 20 (mkStdGen 0) == "VH9SkhtptqPs6GqtofVg"

noRepeat :: Int -> Int -> Bool
noRepeat len n = length (nub $ map (randomString len . mkStdGen) [1..n]) == n


-- For convenience instead of "(undefined :: StdGen)".
g :: StdGen
g = undefined


nonceSpecs :: [Spec]
nonceSpecs = describe "Yesod.Internal.Request.parseWaiRequest (reqNonce)"
  [ it "is Nothing if sessions are disabled" noDisabledNonce
  , it "ignores pre-existing nonce if sessions are disabled" ignoreDisabledNonce
  , it "uses preexisting nonce in session" useOldNonce
  , it "generates a new nonce for sessions without nonce" generateNonce
  ]

noDisabledNonce :: Bool
noDisabledNonce = reqNonce r == Nothing where
  r = parseWaiRequest' defaultRequest [] False g

ignoreDisabledNonce :: Bool
ignoreDisabledNonce = reqNonce r == Nothing where
  r = parseWaiRequest' defaultRequest [("_NONCE", "old")] False g

useOldNonce :: Bool
useOldNonce = reqNonce r == Just "old" where
  r = parseWaiRequest' defaultRequest [("_NONCE", "old")] True g

generateNonce :: Bool
generateNonce = reqNonce r /= Nothing where
  r = parseWaiRequest' defaultRequest [("_NONCE", "old")] True g


langSpecs :: [Spec]
langSpecs = describe "Yesod.Internal.Request.parseWaiRequest (reqLangs)"
  [ it "respects Accept-Language" respectAcceptLangs
  , it "respects sessions" respectSessionLang
  , it "respects cookies" respectCookieLang
  , it "respects queries" respectQueryLang
  , it "prioritizes correctly" prioritizeLangs
  ]

respectAcceptLangs :: Bool
respectAcceptLangs = reqLangs r == ["en-US", "es", "en"] where
  r = parseWaiRequest' defaultRequest
        { requestHeaders = [("Accept-Language", "en-US, es")] } [] False g

respectSessionLang :: Bool
respectSessionLang = reqLangs r == ["en"] where
  r = parseWaiRequest' defaultRequest [("_LANG", "en")] False g

respectCookieLang :: Bool
respectCookieLang = reqLangs r == ["en"] where
  r = parseWaiRequest' defaultRequest
        { requestHeaders = [("Cookie", "_LANG=en")]
        } [] False g

respectQueryLang :: Bool
respectQueryLang = reqLangs r == ["en-US", "en"] where
  r = parseWaiRequest' defaultRequest { queryString = [("_LANG", Just "en-US")] } [] False g

prioritizeLangs :: Bool
prioritizeLangs = reqLangs r == ["en-QUERY", "en-COOKIE", "en-SESSION", "en", "es"] where
  r = parseWaiRequest' defaultRequest
        { requestHeaders = [ ("Accept-Language", "en, es")
                           , ("Cookie", "_LANG=en-COOKIE")
                           ]
        , queryString = [("_LANG", Just "en-QUERY")]
        } [("_LANG", "en-SESSION")] False g


internalRequestTest :: [Spec]
internalRequestTest = descriptions [ randomStringSpecs
                                   , nonceSpecs
                                   , langSpecs
                                   ]
