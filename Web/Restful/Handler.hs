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
    ( Handler
    , liftHandler
    , noHandler
    ) where

import Web.Restful.Request
import Web.Restful.Response

type Handler = Response -- FIXME maybe move some stuff around now...

liftHandler :: (Request req, HasReps rep)
            => (req -> ResponseIO rep)
            -> Handler
liftHandler f = do
    req <- getRequest
    wrapResponse $ f req

noHandler :: Handler
noHandler = notFound
