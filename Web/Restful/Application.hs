{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Object.Text
import Data.Object.String
import Data.Enumerable
import Control.Monad (when)

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

import Data.Convertible
import Control.Arrow ((***))

-- | A data type that can be turned into a Hack application.
class ResourceName a => RestfulApp a where
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

    -- | Output error response pages.
    errorHandler :: Monad m => a -> RawRequest -> ErrorResult -> [RepT m] -- FIXME better type sig?
    errorHandler _ rr NotFound = reps $ toTextObject $
                                   "Not found: " ++ show rr
    errorHandler _ _ (Redirect url) =
        reps $ toTextObject $ "Redirect to: " ++ url
    errorHandler _ _ (InternalError e) =
        reps $ toTextObject $ "Internal server error: " ++ e
    errorHandler _ _ (InvalidArgs ia) =
        reps $ toTextObject $ toStringObject
            [ ("errorMsg", toStringObject "Invalid arguments")
            , ("messages", toStringObject ia)
            ]
    errorHandler _ _ PermissionDenied =
        reps $ toTextObject "Permission denied"

    -- | Whether or not we should check for overlapping resource names.
    checkOverlaps :: a -> Bool
    checkOverlaps = const True

-- | Given a sample resource name (purely for typing reasons), generating
-- a Hack application.
toHackApp :: RestfulApp resourceName
          => resourceName
          -> IO Hack.Application
toHackApp a = do
    when (checkOverlaps a) $ checkResourceName a -- FIXME maybe this should be done compile-time?
    key <- encryptKey a
    let app' = toHackApplication a getHandler
        clientsession' = clientsession [authCookieName] key -- FIXME gotta be a better way...
        app = foldr ($) app' $ hackMiddleware a ++ [clientsession']
    return app

findResourceNames :: ResourceName a
                  => Resource
                  -> [(a, [(String, String)])]
findResourceNames r = takeJusts $ map (checkPatternHelper r) enumerate

checkPatternHelper :: ResourceName a
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

toHackApplication :: RestfulApp resourceName
                  => resourceName
                  -> (resourceName -> Verb -> Handler)
                  -> Hack.Application
toHackApplication sampleRN hm env = do
    -- The following is safe since we run cleanPath as middleware
    let (Right resource) = splitPath $ Hack.pathInfo env
    let (handler :: Handler, urlParams') =
          case findResourceNames resource of
            [] -> (notFound, [])
            ((rn, urlParams''):_) ->
                let verb = toVerb $ Hack.requestMethod env
                 in (hm rn verb, urlParams'')
    let rr = envToRawRequest urlParams' env
    let rawHttpAccept = tryLookup "" "Accept" $ Hack.http env
        ctypes' = parseHttpAccept rawHttpAccept
    r <-
        runHandler handler rr ctypes' >>=
        either (applyErrorHandler sampleRN rr ctypes') return
    responseToHackResponse (rawLanguages rr) r

applyErrorHandler :: (RestfulApp ra, Monad m)
                  => ra
                  -> RawRequest
                  -> [ContentType]
                  -> (ErrorResult, [Header])
                  -> m Response
applyErrorHandler ra rr cts (er, headers) = do
    let (ct, c) = chooseRep cts (errorHandler ra rr er)
    c' <- c
    return $ Response
                (getStatus er)
                (getHeaders er ++ headers)
                ct
                c'

responseToHackResponse :: [String] -- ^ language list
                       -> Response -> IO Hack.Response
responseToHackResponse ls (Response sc hs ct c) = do
    hs' <- mapM toPair hs
    let hs'' = ("Content-Type", ct) : hs'
    let asLBS = runContent ls c
    return $ Hack.Response sc hs'' asLBS

envToRawRequest :: [(ParamName, ParamValue)] -> Hack.Env -> RawRequest
envToRawRequest urlParams' env =
    let (Right rawPieces) = splitPath $ Hack.pathInfo env
        gets' = decodeUrlPairs $ Hack.queryString env :: [(String, String)]
        clength = tryLookup "0" "Content-Length" $ Hack.http env
        ctype = tryLookup "" "Content-Type" $ Hack.http env
        (posts, files) = map (convertSuccess *** convertSuccess) ***
                         map (convertSuccess *** convertFileInfo)
                       $ parsePost ctype clength
                       $ Hack.hackInput env
        rawCookie = tryLookup "" "Cookie" $ Hack.http env
        cookies' = decodeCookies rawCookie :: [(String, String)]
        langs = ["en"] -- FIXME
     in RawRequest rawPieces urlParams' gets' posts cookies' files env langs

convertFileInfo :: ConvertSuccess a b => FileInfo a c -> FileInfo b c
convertFileInfo (FileInfo a b c) =
    FileInfo (convertSuccess a) (convertSuccess b) c
