{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    , HasHandlers (..)
    , liftHandler
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

type HandlerMap a = a -> Verb -> Maybe Handler

class HasHandlers a b | a -> b where
    getHandler :: b -> a -> Verb -> Maybe Handler

liftHandler :: (Request req, Response res)
            => (req -> IO res)
            -> Maybe Handler
liftHandler f = Just . Handler $ fmap ResponseWrapper . f
