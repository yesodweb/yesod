{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Yesod.Helpers.Auth
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
module Yesod.Helpers.Auth
    ( AuthResource
    , authHandler
    , authResourcePattern
    , RpxnowApiKey (..)
    ) where

import qualified Hack
import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId
import Data.Enumerable

import Yesod
import Yesod.Constants

import Control.Applicative ((<$>), Applicative (..))
import Control.Monad.Reader
import Control.Monad.Attempt

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

newtype RpxnowApiKey = RpxnowApiKey String

authHandler :: Maybe RpxnowApiKey -> AuthResource -> Verb -> Handler y HtmlObject
authHandler _ Check Get = authCheck
authHandler _ Logout Get = authLogout
authHandler _ Openid Get = authOpenidForm
authHandler _ OpenidForward Get = authOpenidForward
authHandler _ OpenidComplete Get = authOpenidComplete
-- two different versions of RPX protocol apparently...
authHandler (Just (RpxnowApiKey key)) LoginRpxnow Get = rpxnowLogin key
authHandler (Just (RpxnowApiKey key)) LoginRpxnow Post = rpxnowLogin key
authHandler _ _ _ = notFound

authResourcePattern :: AuthResource -> String -- FIXME supply prefix as well
authResourcePattern Check = "/auth/check/"
authResourcePattern Logout = "/auth/logout/"
authResourcePattern Openid = "/auth/openid/"
authResourcePattern OpenidForward  = "/auth/openid/forward/"
authResourcePattern OpenidComplete = "/auth/openid/complete/"
authResourcePattern LoginRpxnow = "/auth/login/rpxnow/"


data OIDFormReq = OIDFormReq (Maybe String) (Maybe String)
instance Request OIDFormReq where
    parseRequest = OIDFormReq <$> getParam "message" <*> getParam "dest"
instance ConvertSuccess OIDFormReq Html where
    convertSuccess (OIDFormReq Nothing _) = cs ""
    convertSuccess (OIDFormReq (Just s) _) =
        Tag "p" [("class", "message")] [cs s]

authOpenidForm :: Handler y HtmlObject
authOpenidForm = do
    m@(OIDFormReq _ dest) <- parseRequest
    let html =
         HtmlList
          [ cs m
          , Tag "form" [("method", "get"), ("action", "forward/")]
                [ Tag "label" [("for", "openid")] [cs "OpenID: "]
                , EmptyTag "input" [("type", "submit"), ("value", "Login")]
                ]
          ]
    case dest of
        Just dest' -> addCookie 120 "DEST" dest'
        Nothing -> return ()
    return $ toHtmlObject $ Html $ cs html

authOpenidForward :: Handler y HtmlObject
authOpenidForward = do
    oid <- getParam "openid"
    env <- parseEnv
    let complete = "http://" ++ Hack.serverName env ++ ":" ++
                   show (Hack.serverPort env) ++
                   "/auth/openid/complete/"
    res <- runAttemptT $ OpenId.getForwardUrl oid complete
    attempt
      (\err -> redirect $ "/auth/openid/?message=" ++ encodeUrl (show err))
      redirect
      res

authOpenidComplete :: Handler y HtmlObject
authOpenidComplete = do
    gets' <- rawGetParams <$> askRawRequest
    dest <- cookieParam "DEST"
    res <- runAttemptT $ OpenId.authenticate gets'
    let onFailure err = redirect $ "/auth/openid/?message="
                            ++ encodeUrl (show err)
    let onSuccess (OpenId.Identifier ident) = do
        deleteCookie "DEST"
        header authCookieName ident
        redirect $ fromMaybe "/" dest
    attempt onFailure onSuccess res

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
            -> Handler y HtmlObject
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
    ident <- Rpxnow.authenticate apiKey token
    header authCookieName $ Rpxnow.identifier ident
    redirect dest

authCheck :: Handler y HtmlObject
authCheck = do
    ident <- maybeIdentifier
    case ident of
        Nothing -> return $ toHtmlObject [("status", "notloggedin")]
        Just i -> return $ toHtmlObject
            [ ("status", "loggedin")
            , ("ident", i)
            ]

authLogout :: Handler y HtmlObject
authLogout = do
    deleteCookie authCookieName
    return $ toHtmlObject [("status", "loggedout")]
