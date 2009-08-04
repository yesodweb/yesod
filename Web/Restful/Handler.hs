{-# LANGUAGE ExistentialQuantification #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Handler
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : unstable
-- Portability   : portable
--
-- Define Handler stuff.
--
---------------------------------------------------------
module Web.Restful.Handler
    ( Handler (..)
    , runHandler
    , HandlerMap
    , HandlerDesc (..)
    ) where

import Web.Restful.Definitions
import Web.Restful.Request
import Web.Restful.Response

data Handler = forall req. Request req => Handler (req -> IO ResponseWrapper)

runHandler :: Handler -> RawRequest -> IO ResponseWrapper
runHandler (Handler f) rreq = do
    let rparser = parseRequest
    case runRequestParser rparser rreq of
        Left errors -> fail $ unlines errors -- FIXME
        Right req -> f req

data HandlerDesc a = HandlerDesc a Verb Handler
type HandlerMap a = [HandlerDesc a]
