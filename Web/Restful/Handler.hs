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

type Handler = RawRequest -> Response

liftHandler :: (Request req, HasReps rep)
            => (req -> ResponseIO rep)
            -> Handler
liftHandler f req = liftRequest req >>= wrapResponse . f

liftRequest :: (Request req, Monad m) => RawRequest -> m req
liftRequest r =
    case runRequestParser parseRequest r of
        Left errors -> fail $ unlines errors -- FIXME
        Right req -> return req

noHandler :: Handler
noHandler = const notFound
