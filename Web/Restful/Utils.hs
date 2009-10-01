---------------------------------------------------------
--
-- Module        : Web.Restful.Utils
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Utility functions for Restful.
-- These are all functions which could be exported to another library.
--
---------------------------------------------------------
module Web.Restful.Utils
    ( parseHttpAccept
    , tryLookup
    , formatW3
    , testSuite
    ) where

import Data.List.Split (splitOneOf)
import Data.Maybe (fromMaybe)

import Data.Time.Clock
import System.Locale
import Data.Time.Format

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: String -> [String]
parseHttpAccept = filter (not . specialHttpAccept) . splitOneOf ";,"

specialHttpAccept :: String -> Bool
specialHttpAccept ('q':'=':_) = True
specialHttpAccept ('*':_) = True
specialHttpAccept _ = False

-- | Attempt a lookup, returning a default value on failure.
tryLookup :: Eq k => v -> k -> [(k, v)] -> v
tryLookup def key = fromMaybe def . lookup key

-- | Format a 'UTCTime' in W3 format; useful for setting cookies.
formatW3 :: UTCTime -> String
formatW3 = formatTime defaultTimeLocale "%FT%X-00:00"

----- Testing
testSuite :: Test
testSuite = testGroup "Web.Restful.Response"
    [ testCase "tryLookup1" test_tryLookup1
    , testCase "tryLookup2" test_tryLookup2
    ]

test_tryLookup1 :: Assertion
test_tryLookup1 = tryLookup "default" "foo" [] @?= "default"

test_tryLookup2 :: Assertion
test_tryLookup2 = tryLookup "default" "foo" [("foo", "baz")] @?= "baz"
