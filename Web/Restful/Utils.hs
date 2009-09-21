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
    ) where

import Data.List.Split (splitOneOf)
import Data.Maybe (fromMaybe)

import Data.Time.Clock
import System.Locale
import Data.Time.Format

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
formatW3 = formatTime defaultTimeLocale "%FT%X-08:00" -- FIXME time zone?
