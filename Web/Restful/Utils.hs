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
--
---------------------------------------------------------
module Web.Restful.Utils
    ( parseHttpAccept
    , tryLookup
    ) where

import Data.List.Split (splitOneOf)

parseHttpAccept :: String -> [String]
parseHttpAccept = filter (not . specialHttpAccept) . splitOneOf ";,"

specialHttpAccept :: String -> Bool
specialHttpAccept ('q':'=':_) = True
specialHttpAccept ('*':_) = True
specialHttpAccept _ = False

tryLookup :: Eq k => v -> k -> [(k, v)] -> v
tryLookup v _ [] = v
tryLookup v k ((k', v'):rest)
    | k == k' = v'
    | otherwise = tryLookup v k rest
