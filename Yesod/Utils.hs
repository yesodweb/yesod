{-# LANGUAGE CPP #-}
---------------------------------------------------------
--
-- Module        : Yesod.Utils
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
module Yesod.Utils
    ( parseHttpAccept
    , tryLookup
#if TEST
    , testSuite
#endif
    ) where

import Data.List.Split (splitOneOf)
import Data.Maybe (fromMaybe)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

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

#if TEST
----- Testing
testSuite :: Test
testSuite = testGroup "Yesod.Utils"
    [ testCase "tryLookup1" caseTryLookup1
    , testCase "tryLookup2" caseTryLookup2
    ]

caseTryLookup1 :: Assertion
caseTryLookup1 = tryLookup "default" "foo" [] @?= "default"

caseTryLookup2 :: Assertion
caseTryLookup2 = tryLookup "default" "foo" [("foo", "baz")] @?= "baz"
#endif
