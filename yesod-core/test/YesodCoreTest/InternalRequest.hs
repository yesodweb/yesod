{-# LANGUAGE OverloadedStrings #-}
module YesodCoreTest.InternalRequest (internalRequestTest) where

import Data.List (nub)
import System.Random (StdGen, mkStdGen)

import Network.Wai as W
import Network.Wai.Test
import Yesod.Core.Internal (randomString, parseWaiRequest)
import Test.Hspec
import Data.Monoid (mempty)
import Data.Map (singleton)
import Yesod.Core
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random.MWC as MWC
import Control.Monad.ST
import Control.Monad (replicateM)

randomStringSpecs :: Spec
randomStringSpecs = describe "Yesod.Internal.Request.randomString" $ do
    --it "looks reasonably random" looksRandom
    it "does not repeat itself" $ noRepeat 10 100

-- NOTE: this testcase may break on other systems/architectures if
-- mkStdGen is not identical everywhere (is it?).
looksRandom :: Bool
looksRandom = runST $ do
    gen <- MWC.create
    s <- randomString 20 gen
    return $ s == "VH9SkhtptqPs6GqtofVg"

noRepeat :: Int -> Int -> Bool
noRepeat len n = runST $ do
    gen <- MWC.create
    ss <- replicateM n $ randomString len gen
    return $ length (nub ss) == n


-- For convenience instead of "(undefined :: StdGen)".
g :: MWC.GenIO
g = error "test/YesodCoreTest/InternalRequest.g"

parseWaiRequest' :: Request
                 -> SessionMap
                 -> Bool
                 -> Word64
                 -> YesodRequest
parseWaiRequest' a b c d = unsafePerformIO $ -- ugly hack, just to ease migration, should be removed
    case parseWaiRequest a b c (Just d) of
        Left yreq -> yreq
        Right needGen -> needGen g

tokenSpecs :: Spec
tokenSpecs = describe "Yesod.Internal.Request.parseWaiRequest (reqToken)" $ do
    it "is Nothing if sessions are disabled" noDisabledToken
    it "ignores pre-existing token if sessions are disabled" ignoreDisabledToken
    it "uses preexisting token in session" useOldToken
    it "generates a new token for sessions without token" generateToken

noDisabledToken :: Bool
noDisabledToken = reqToken r == Nothing where
  r = parseWaiRequest' defaultRequest mempty False 1000

ignoreDisabledToken :: Bool
ignoreDisabledToken = reqToken r == Nothing where
  r = parseWaiRequest' defaultRequest (singleton "_TOKEN" "old") False 1000

useOldToken :: Bool
useOldToken = reqToken r == Just "old" where
  r = parseWaiRequest' defaultRequest (singleton "_TOKEN" "old") True 1000

generateToken :: Bool
generateToken = reqToken r /= Nothing where
  r = parseWaiRequest' defaultRequest (singleton "_TOKEN" "old") True 1000


langSpecs :: Spec
langSpecs = describe "Yesod.Internal.Request.parseWaiRequest (reqLangs)" $ do
    it "respects Accept-Language" respectAcceptLangs
    it "respects sessions" respectSessionLang
    it "respects cookies" respectCookieLang
    it "respects queries" respectQueryLang
    it "prioritizes correctly" prioritizeLangs

respectAcceptLangs :: Bool
respectAcceptLangs = reqLangs r == ["en-US", "es", "en"] where
  r = parseWaiRequest' defaultRequest
        { requestHeaders = [("Accept-Language", "en-US, es")] } mempty False 1000

respectSessionLang :: Bool
respectSessionLang = reqLangs r == ["en"] where
  r = parseWaiRequest' defaultRequest (singleton "_LANG" "en") False 1000

respectCookieLang :: Bool
respectCookieLang = reqLangs r == ["en"] where
  r = parseWaiRequest' defaultRequest
        { requestHeaders = [("Cookie", "_LANG=en")]
        } mempty False 1000

respectQueryLang :: Bool
respectQueryLang = reqLangs r == ["en-US", "en"] where
  r = parseWaiRequest' defaultRequest { queryString = [("_LANG", Just "en-US")] } mempty False 1000

prioritizeLangs :: Bool
prioritizeLangs = reqLangs r == ["en-QUERY", "en-COOKIE", "en-SESSION", "en", "es"] where
  r = parseWaiRequest' defaultRequest
        { requestHeaders = [ ("Accept-Language", "en, es")
                           , ("Cookie", "_LANG=en-COOKIE")
                           ]
        , queryString = [("_LANG", Just "en-QUERY")]
        } (singleton "_LANG" "en-SESSION") False 10000

internalRequestTest :: Spec
internalRequestTest = describe "Test.InternalRequestTest" $ do
      randomStringSpecs
      tokenSpecs
      langSpecs
