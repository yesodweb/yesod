{-# LANGUAGE OverloadedStrings #-}
module Test.InternalRequest (internalRequestTest) where

import Data.List (nub)
import System.Random (StdGen, mkStdGen)

import Network.Wai as W
import Network.Wai.Test
import Yesod.Internal.TestApi
import Test.Hspec

randomStringSpecs :: [Spec]
randomStringSpecs = describe "Yesod.Internal.Request.randomString"
  [ it "does not repeat itself" $ noRepeat 10 100
  ]

noRepeat len n = length (nub $ map (randomString len . mkStdGen) [1..n]) == n


-- For convenience instead of "(undefined :: StdGen)".
g :: StdGen
g = undefined


nonceSpecs :: [Spec]
nonceSpecs = describe "Yesod.Internal.Request.parseWaiRequest (reqNonce)"
  [ it "is Nothing for unsecure sessions" noUnsecureNonce
  , it "ignores pre-existing nonce for unsecure sessions" ignoreUnsecureNonce
  , it "uses preexisting nonce for secure sessions" useOldNonce
  , it "generates a new nonce for secure sessions without nonce" generateNonce
  ]

noUnsecureNonce = reqNonce r == Nothing where
  r = parseWaiRequest' defaultRequest [] Nothing g

ignoreUnsecureNonce = reqNonce r == Nothing where
  r = parseWaiRequest' defaultRequest [("_NONCE", "old")] Nothing g

useOldNonce = reqNonce r == Just "old" where
  r = parseWaiRequest' defaultRequest [("_NONCE", "old")] (Just undefined) g

generateNonce = reqNonce r /= Nothing where
  r = parseWaiRequest' defaultRequest [("_NONCE", "old")] (Just undefined) g


langSpecs :: [Spec]
langSpecs = describe "Yesod.Internal.Request.parseWaiRequest (reqLangs)"
  [ it "respects Accept-Language" respectAcceptLangs
  , it "respects sessions" respectSessionLang
  , it "respects cookies" respectCookieLang
  , it "respects queries" respectQueryLang
  , it "prioritizes correctly" prioritizeLangs
  ]

respectAcceptLangs = reqLangs r == ["accept1", "accept2"] where
  r = parseWaiRequest' defaultRequest
        { requestHeaders = [("Accept-Language", "accept1, accept2")] } [] Nothing g

respectSessionLang = reqLangs r == ["session"] where
  r = parseWaiRequest' defaultRequest [("_LANG", "session")] Nothing g

respectCookieLang = reqLangs r == ["cookie"] where
  r = parseWaiRequest' defaultRequest
        { requestHeaders = [("Cookie", "_LANG=cookie")]
        } [] Nothing g

respectQueryLang = reqLangs r == ["query"] where
  r = parseWaiRequest' defaultRequest { queryString = [("_LANG", Just "query")] } [] Nothing g

prioritizeLangs = reqLangs r == ["query", "cookie", "session", "accept1", "accept2"] where
  r = parseWaiRequest' defaultRequest
        { requestHeaders = [ ("Accept-Language", "accept1, accept2")
                           , ("Cookie", "_LANG=cookie")
                           ]
        , queryString = [("_LANG", Just "query")]
        } [("_LANG", "session")] Nothing g


internalRequestTest :: [Spec]
internalRequestTest = descriptions [ randomStringSpecs
                                   , nonceSpecs
                                   , langSpecs
                                   ]

main = hspec internalRequestTest
