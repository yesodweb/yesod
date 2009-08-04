---------------------------------------------------------
-- |
-- Module        : Hack.Middleware.MethodOverride
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Override the HTTP method based on either:
--   The X-HTTP-Method-Override header.
--   The _method_override GET parameter.
--
---------------------------------------------------------
module Hack.Middleware.MethodOverride (methodOverride) where

import Hack
import Web.Encodings (decodeUrlPairs)
import Data.Monoid (mappend)

methodOverride :: Middleware
methodOverride app env = do
    let mo1 = lookup "X-HTTP-Method-Override" $ http env
        gets = decodeUrlPairs $ queryString env
        mo2 = lookup "_method_override" gets
        cm = requestMethod env
    app $
        case mo1 `mappend` mo2 of
            Nothing -> env
            Just nm -> env { requestMethod = safeRead cm nm }

safeRead :: Read a => a -> String -> a
safeRead d s =
  case reads s of
    ((x, _):_) -> x
    [] -> d
