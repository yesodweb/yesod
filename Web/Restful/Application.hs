{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Application
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Defining the application.
--
---------------------------------------------------------
module Web.Restful.Application
    (
      toHackApp
    , RestfulApp (..)
    ) where

import Web.Encodings
import Data.ByteString.Class
import qualified Data.ByteString.Lazy as B

import qualified Hack
import Hack.Middleware.CleanPath
import Hack.Middleware.ClientSession
import Hack.Middleware.Gzip
import Hack.Middleware.Jsonp
import Hack.Middleware.MethodOverride

import Web.Restful.Request
import Web.Restful.Response
import Web.Restful.Utils
import Web.Restful.Handler
import Web.Restful.Definitions
import Web.Restful.Constants
import Web.Restful.Resource

-- | A data type that can be turned into a Hack application.
class ResourceName a b => RestfulApp a b | a -> b where
    -- | Load up the model, ie the data which use passed to each handler.
    getModel :: a -> IO b

    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO Word256
    encryptKey _ = getKey defaultKeyFile

    -- | All of the middlewares to install.
    hackMiddleware :: a -> [Hack.Middleware]
    hackMiddleware _ =
                [ gzip
                , cleanPath
                , jsonp
                , methodOverride
                ]

    -- | How to generate 404 pages. FIXME make more user-friendly.
    response404 :: a -> Hack.Env -> IO Hack.Response
    response404 _ = default404

    -- | Wrappers for cleaning up responses. Especially intended for
    -- beautifying static HTML. FIXME more user friendly.
    responseWrapper :: a -> String -> B.ByteString -> IO B.ByteString
    responseWrapper _ _ = return

-- | Given a sample resource name (purely for typing reasons), generating
-- a Hack application.
toHackApp :: RestfulApp resourceName modelType
          => resourceName
          -> IO Hack.Application
toHackApp a = do
    model <- getModel a
    key <- encryptKey a
    let handlers = getHandler model
        app' = toHackApplication a handlers
        clientsession' = clientsession [authCookieName] key -- FIXME gotta be a better way...
        app = foldr ($) app' $ hackMiddleware a ++ [clientsession']
    return app

findResourceNames :: ResourceName a model
                  => Resource
                  -> [(a, [(String, String)])]
findResourceNames r = takeJusts $ map (checkPatternHelper r) allValues

checkPatternHelper :: ResourceName a model
                   => Resource
                   -> a
                   -> Maybe (a, [(String, String)])
checkPatternHelper r rn =
    case checkPattern (fromString $ resourcePattern rn) r of
        Nothing -> Nothing
        Just pairs -> Just (rn, pairs)

takeJusts :: [Maybe a] -> [a]
takeJusts [] = []
takeJusts (Nothing:rest) = takeJusts rest
takeJusts (Just x:rest) = x : takeJusts rest

toHackApplication :: RestfulApp resourceName model
                  => resourceName
                  -> (resourceName -> Verb -> Handler)
                  -> Hack.Application
toHackApplication sampleRN hm env = do
    let (Right resource) = splitPath $ Hack.pathInfo env
    case findResourceNames resource of
      [] -> response404 sampleRN $ env
      [(rn, urlParams')] -> do
        let verb :: Verb
            verb = toVerb $ Hack.requestMethod env
            rr :: RawRequest
            rr = envToRawRequest urlParams' env
            handler :: Handler
            handler = hm rn verb
        let rawHttpAccept = tryLookup "" "Accept" $ Hack.http env
            ctypes' = parseHttpAccept rawHttpAccept
        runResponse (handler rr) ctypes'
      x -> error $ "Invalid matches: " ++ show x

envToRawRequest :: [(ParamName, ParamValue)] -> Hack.Env -> RawRequest
envToRawRequest urlParams' env =
    let (Right rawPieces) = splitPath $ Hack.pathInfo env
        gets' = decodeUrlPairs $ Hack.queryString env :: [(String, String)]
        clength = tryLookup "0" "Content-Length" $ Hack.http env
        ctype = tryLookup "" "Content-Type" $ Hack.http env
        (posts, files) = parsePost ctype clength
                       $ Hack.hackInput env
        rawCookie = tryLookup "" "Cookie" $ Hack.http env
        cookies' = decodeCookies rawCookie :: [(String, String)]
     in RawRequest rawPieces urlParams' gets' posts cookies' files env

default404 :: Hack.Env -> IO Hack.Response
default404 env = return $
    Hack.Response
        404
        [("Content-Type", "text/plain")]
        $ toLazyByteString $ "Not found: " ++ Hack.pathInfo env
