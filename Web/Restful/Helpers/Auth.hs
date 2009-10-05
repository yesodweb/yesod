{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Helpers.Auth
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Authentication through the authentication package.
--
---------------------------------------------------------
module Web.Restful.Helpers.Auth
    ( AuthResource
    ) where

import qualified Hack
import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId
import Data.Enumerable

import Web.Restful
import Web.Restful.Constants

import Control.Applicative ((<$>), Applicative (..))
import Control.Monad.Reader

import Data.Maybe (fromMaybe)

data AuthResource =
    Check
    | Logout
    | Openid
    | OpenidForward
    | OpenidComplete
    | LoginRpxnow
    deriving Show

instance Enumerable AuthResource where
    enumerate =
        [ Check
        , Logout
        , Openid
        , OpenidForward
        , OpenidComplete
        , LoginRpxnow
        ]

type RpxnowApiKey = String -- FIXME newtype
instance ResourceName AuthResource (Maybe RpxnowApiKey) where
    getHandler _ Check Get = authCheck
    getHandler _ Logout Get = authLogout
    getHandler _ Openid Get = authOpenidForm
    getHandler _ OpenidForward Get = authOpenidForward
    getHandler _ OpenidComplete Get = authOpenidComplete
    -- two different versions of RPX protocol apparently...
    getHandler (Just key) LoginRpxnow Get = rpxnowLogin key
    getHandler (Just key) LoginRpxnow Post = rpxnowLogin key
    getHandler _ _ _ = notFound

    resourcePattern Check = "/auth/check/"
    resourcePattern Logout = "/auth/logout/"
    resourcePattern Openid = "/auth/openid/"
    resourcePattern OpenidForward  = "/auth/openid/forward/"
    resourcePattern OpenidComplete = "/auth/openid/complete/"
    resourcePattern LoginRpxnow = "/auth/login/rpxnow/"


data OIDFormReq = OIDFormReq (Maybe String) (Maybe String)
instance Request OIDFormReq where
    parseRequest = OIDFormReq <$> getParam "message" <*> getParam "dest"
instance Show OIDFormReq where
    show (OIDFormReq Nothing _) = ""
    show (OIDFormReq (Just s) _) = "<p class='message'>" ++ encodeHtml s ++
                                 "</p>"

authOpenidForm :: Handler
authOpenidForm = do
    m@(OIDFormReq _ dest) <- parseRequest
    let html =
            show m ++
            "<form method='get' action='forward/'>" ++
            "OpenID: <input type='text' name='openid'>" ++
            "<input type='submit' value='Login'>" ++
            "</form>"
    case dest of
        Just dest' -> addCookie 120 "DEST" dest'
        Nothing -> return ()
    htmlResponse html

authOpenidForward :: Handler
authOpenidForward = do
    oid <- getParam "openid"
    env <- parseEnv
    let complete = "http://" ++ Hack.serverName env ++ ":" ++
                   show (Hack.serverPort env) ++
                   "/auth/openid/complete/"
    res <- liftIO $ OpenId.getForwardUrl oid complete
    case res of
        Left err -> redirect $ "/auth/openid/?message="
                            ++ encodeUrl (err :: String)
        Right url -> redirect url

authOpenidComplete :: Handler
authOpenidComplete = do
    gets' <- rawGetParams <$> askRawRequest
    dest <- cookieParam "DEST"
    res <- liftIO $ OpenId.authenticate gets'
    case res of
      Left err -> redirect $ "/auth/openid/?message="
                          ++ encodeUrl (err :: String)
      Right (OpenId.Identifier ident) -> do
        deleteCookie "DEST"
        header authCookieName ident
        redirect $ fromMaybe "/" dest

-- | token dest
data RpxnowRequest = RpxnowRequest String (Maybe String)
instance Request RpxnowRequest where
    parseRequest = do
        token <- anyParam "token"
        dest <- anyParam "dest"
        return $! RpxnowRequest token $ chopHash `fmap` dest

chopHash :: String -> String
chopHash ('#':rest) = rest
chopHash x = x

rpxnowLogin :: String -- ^ api key
            -> Handler
rpxnowLogin apiKey = do
    token <- anyParam "token"
    postDest <- postParam "dest"
    dest' <- case postDest of
                Nothing -> getParam "dest"
                Just d -> return d
    let dest = case dest' of
                Nothing -> "/"
                Just "" -> "/"
                Just ('#':rest) -> rest
                Just s -> s
    ident <- join $ liftIO $ Rpxnow.authenticate apiKey token
    header authCookieName $ Rpxnow.identifier ident
    redirect dest

authCheck :: Handler
authCheck = do
    ident <- maybeIdentifier
    case ident of
        Nothing -> objectResponse [("status", "notloggedin")]
        Just i -> objectResponse
            [ ("status", "loggedin")
            , ("ident", i)
            ]

authLogout :: Handler
authLogout = do
    deleteCookie authCookieName
    objectResponse [("status", "loggedout")]
