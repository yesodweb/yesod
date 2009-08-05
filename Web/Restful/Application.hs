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
      -- * Defining an application
      ApplicationMonad
      -- ** Settings
    , setHtmlWrapper
      -- ** Engage
    , toHackApp
    ) where

-- hideously long import list
import qualified Hack
import Control.Monad.State hiding (gets)
import Web.Encodings
import Data.Maybe (isJust)
import Data.ByteString.Class
import qualified Data.ByteString.Lazy as BS
import Data.Function.Predicate (equals)
import Data.Default
import Control.Applicative ( Applicative (..))

import Hack.Middleware.Gzip
import Hack.Middleware.CleanPath
import Hack.Middleware.Jsonp
import Hack.Middleware.ClientSession
import Hack.Middleware.MethodOverride

import Web.Restful.Request
import Web.Restful.Response
import Web.Restful.Utils
import Web.Restful.Handler
import Web.Restful.Definitions
import Web.Restful.Constants

-- | Contains settings and a list of resources.
type ApplicationMonad a = State (ApplicationSettings a)
instance Applicative (ApplicationMonad a) where
    pure = return
    f <*> a = do
        f' <- f
        a' <- a
        return $! f' a'
data ApplicationSettings rn = ApplicationSettings
    { encryptKey :: Either FilePath Word256
    , hackMiddleware :: [Hack.Middleware]
    , response404 :: Hack.Env -> IO Hack.Response
    , htmlWrapper :: BS.ByteString -> BS.ByteString
    }

instance (HasResourceParser a) =>
        Default (ApplicationSettings a) where
    def = ApplicationSettings
            { encryptKey = Left defaultKeyFile
            , hackMiddleware =
                [ gzip
                , cleanPath
                , jsonp
                , methodOverride
                ]
            , response404 = default404
            , htmlWrapper = id
            }

default404 :: Hack.Env -> IO Hack.Response
default404 env = return $
    Hack.Response
        404
        [("Content-Type", "text/plain")]
        $ toLazyByteString $ "Not found: " ++ Hack.pathInfo env

-- FIXME document below here

setHtmlWrapper :: (BS.ByteString -> BS.ByteString) -> ApplicationMonad a ()
setHtmlWrapper f = do
    s <- get
    put $ s { htmlWrapper = f }

toHackApp :: (Eq a, HasResourceParser a, HasHandlers a b)
          => ApplicationMonad a ()
          -> b
          -> IO Hack.Application
toHackApp am model = do
    let settings = execState am def
    key <- case encryptKey settings of
            Left f -> getKey f
            Right k -> return k
    let handlers = getHandler model
        app' = toHackApplication handlers settings
        clientsession' = clientsession [authCookieName] key -- FIXME gotta be a better way...
        app = foldr ($) app' $ hackMiddleware settings ++ [clientsession']
    return app

toHackApplication :: (HasResourceParser resourceName, Eq resourceName)
                  => HandlerMap resourceName
                  -> ApplicationSettings resourceName
                  -> Hack.Application
toHackApplication hm settings env = do
    let (Right resource) = splitPath $ Hack.pathInfo env
    case resourceParser resource of
      Nothing -> response404 settings $ env
      (Just (ParsedResource rn urlParams')) -> do
        let verb :: Verb
            verb = toVerb $ Hack.requestMethod env
            rr :: RawRequest
            rr = envToRawRequest urlParams' env
        case hm rn verb of
            (Just handler) -> do
                let rawHttpAccept = tryLookup "" "Accept" $ Hack.http env
                    ctypes' = parseHttpAccept rawHttpAccept
                body <- runHandler handler rr
                let reps' = reps body
                    ctypes = filter (\c -> isJust $ lookup c reps') ctypes'
                let handlerPair =
                      case ctypes of
                        [] -> Just $ head reps'
                        (c:_) ->
                          case filter (fst `equals` c) reps' of
                            [pair] -> Just pair
                            [] -> Nothing
                            _ -> error "Overlapping reps"
                case handlerPair of
                    Nothing -> response404 settings $ env
                    Just (ctype, Hack.Response status headers content) -> do
                        let wrapper =
                                case ctype of
                                    "text/html" -> htmlWrapper settings
                                    _ -> id
                        return $ Hack.Response status
                                 (("Content-Type", ctype) : headers)
                               $ toLazyByteString $ wrapper content
            Nothing -> response404 settings $ env

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
