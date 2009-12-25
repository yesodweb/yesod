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
    ( authHandler
    , YesodAuth (..)
    ) where

import qualified Hack
import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Yesod
import Yesod.Constants

import Control.Applicative ((<$>), Applicative (..))
import Control.Monad.Attempt

import Data.Maybe (fromMaybe)

class Yesod a => YesodAuth a where
    rpxnowApiKey :: a -> Maybe String
    rpxnowApiKey _ = Nothing

data AuthResource =
    Check
    | Logout
    | Openid
    | OpenidForward
    | OpenidComplete
    | LoginRpxnow
    deriving (Show, Eq, Enum, Bounded)

rc :: HasReps x => Handler y x -> Handler y RepChooser
rc = fmap chooseRep

authHandler :: YesodAuth y => Verb -> [String] -> Handler y RepChooser
authHandler Get ["check"] = rc authCheck
authHandler Get ["logout"] = rc authLogout
authHandler Get ["openid"] = rc authOpenidForm
authHandler Get ["openid", "forward"] = rc authOpenidForward
authHandler Get ["openid", "complete"] = rc authOpenidComplete
-- two different versions of RPX protocol apparently, so just accepting all
-- verbs
authHandler _ ["login", "rpxnow"] = rc rpxnowLogin
authHandler _ _ = notFound

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
                , EmptyTag "input" [("type", "text"), ("id", "openid"),
                                    ("name", "openid")]
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

rpxnowLogin :: YesodAuth y => Handler y HtmlObject
rpxnowLogin = do
    ay <- getYesod
    apiKey <- case rpxnowApiKey ay of
                Just x -> return x
                Nothing -> notFound
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
