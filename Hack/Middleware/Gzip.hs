---------------------------------------------------------
-- |
-- Module        : Hack.Middleware.Gzip
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic gzip compression of responses.
--
---------------------------------------------------------
module Hack.Middleware.Gzip (gzip) where

import Hack
import Codec.Compression.GZip (compress)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOneOf)

-- | Use gzip to compress the body of the response.
--
-- Analyzes the \"Accept-Encoding\" header from the client to determine
-- if gzip is supported.
--
-- Possible future enhancements:
--
-- * Only compress if the response is above a certain size.
--
-- * Add Content-Length.
--
-- * I read somewhere that \"the beast\" (MSIE) can\'t support compression
-- for Javascript files..
gzip :: Middleware
gzip app env = do
    res <- app env
    let enc = fromMaybe [] $ splitOneOf "," `fmap` lookup "Accept-Encoding"
              (http env)
    if "gzip" `elem` enc
        then return res
                { body = compress $ body res
                , headers = ("Content-Encoding", "gzip") : headers res
                }
        else return res
