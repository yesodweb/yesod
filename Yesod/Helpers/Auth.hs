{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
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
    ( maybeIdentifier
    , authIdentifier
    , displayName
    , redirectLogin
    , Auth (..)
    , AuthRoutes (..)
    , siteAuth
    , YesodAuth (..)
    , identKey
    , displayNameKey
    ) where

import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Yesod
import Data.Convertible.Text

import Control.Monad.Attempt
import Data.Maybe
import Control.Applicative

import Data.Typeable (Typeable)
import Control.Exception (Exception)

class Yesod master => YesodAuth master where
    defaultDest :: master -> Routes master

    defaultLoginRoute :: master -> Routes master

    onRpxnowLogin :: Rpxnow.Identifier -> GHandler Auth master ()
    onRpxnowLogin _ = return ()

data Auth = Auth
    { authIsOpenIdEnabled :: Bool
    , authRpxnowApiKey :: Maybe String
    }

$(mkYesodSub "Auth" [''YesodAuth] [$parseRoutes|
/check                 Check              GET
/logout                Logout             GET
/openid                OpenIdR            GET
/openid/forward        OpenIdForward      GET
/openid/complete       OpenIdComplete     GET
/login/rpxnow          RpxnowR
|])

data ExpectedSingleParam = ExpectedSingleParam
    deriving (Show, Typeable)
instance Exception ExpectedSingleParam

getOpenIdR :: Yesod master => GHandler Auth master RepHtml
getOpenIdR = do
    rr <- getRequest
    case getParams rr "dest" of
        [] -> return ()
        (x:_) -> setUltDestString x
    rtom <- getRouteToMaster
    let message = cs <$> (listToMaybe $ getParams rr "message")
    applyLayout "Log in via OpenID" (return ()) [$hamlet|
$maybe message msg
    %p.message $msg$
%form!method=get!action=@rtom.OpenIdForward@
    %label!for=openid OpenID: 
    %input#openid!type=text!name=openid
    %input!type=submit!value=Login
|]

getOpenIdForward :: GHandler Auth master ()
getOpenIdForward = do
    rr <- getRequest
    oid <- case getParams rr "openid" of
            [x] -> return x
            _ -> invalidArgs [("openid", show ExpectedSingleParam)]
    render <- getUrlRender
    toMaster <- getRouteToMaster
    let complete = render $ toMaster OpenIdComplete
    res <- runAttemptT $ OpenId.getForwardUrl oid complete
    attempt
      (\err -> redirectParams RedirectTemporary (toMaster OpenIdR)
                [("message", show err)])
      (redirectString RedirectTemporary)
      res

getOpenIdComplete :: YesodAuth master => GHandler Auth master ()
getOpenIdComplete = do
    rr <- getRequest
    let gets' = reqGetParams rr
    res <- runAttemptT $ OpenId.authenticate gets'
    renderm <- getUrlRender
    toMaster <- getRouteToMaster
    let render = renderm . toMaster
    let errurl err = render OpenIdR ++ "?message=" ++ encodeUrl (show err)
    let onFailure err = redirectString RedirectTemporary $ errurl err
    let onSuccess (OpenId.Identifier ident) = do
        y <- getYesod
        setSession identKey ident
        redirectUltDest RedirectTemporary $ defaultDest y
    attempt onFailure onSuccess res

handleRpxnowR :: YesodAuth master => GHandler Auth master ()
handleRpxnowR = do
    ay <- getYesod
    auth <- getYesodSub
    apiKey <- case authRpxnowApiKey auth of
                Just x -> return x
                Nothing -> notFound
    rr <- getRequest
    pp <- postParams rr
    let token = case getParams rr "token" ++ pp "token" of
                    [] -> failure MissingToken
                    (x:_) -> x
    ident <- liftIO $ Rpxnow.authenticate apiKey token
    onRpxnowLogin ident
    setSession identKey $ Rpxnow.identifier ident
    setSession displayNameKey $ getDisplayName ident
    either (redirect RedirectTemporary) (redirectString RedirectTemporary) $
        case pp "dest" of
            (d:_) -> Right d
            [] -> case getParams rr "dest" of
                    [] -> Left $ defaultDest ay
                    ("":_) -> Left $ defaultDest ay
                    (('#':rest):_) -> Right rest
                    (s:_) -> Right s


data MissingToken = MissingToken
    deriving (Show, Typeable)
instance Exception MissingToken

-- | Get some form of a display name, defaulting to the identifier.
getDisplayName :: Rpxnow.Identifier -> String
getDisplayName (Rpxnow.Identifier ident extra) = helper choices where
    choices = ["verifiedEmail", "email", "displayName", "preferredUsername"]
    helper [] = ident
    helper (x:xs) = fromMaybe (helper xs) $ lookup x extra

getCheck :: Yesod master => GHandler Auth master RepHtmlJson
getCheck = do
    ident <- maybeIdentifier
    dn <- displayName
    let arg = (cs $ fromMaybe "" ident, cs $ fromMaybe "" dn)
    applyLayoutJson "Authentication Status" (return ()) arg html json
  where
    html (x, y) = [$hamlet|
%h1 Authentication Status
%dl
    %dt identifier
    %dd $x$
    %dt displayName
    %dd $y$
|]
    json (ident, dn) =
        jsonMap [ ("ident", jsonScalar ident)
                , ("displayName", jsonScalar dn)
                ]

getLogout :: YesodAuth master => GHandler Auth master ()
getLogout = do
    y <- getYesod
    clearSession identKey
    redirectUltDest RedirectTemporary $ defaultDest y

-- | Gets the identifier for a user if available.
maybeIdentifier :: RequestReader m => m (Maybe String)
maybeIdentifier = do
    s <- session
    return $ listToMaybe $ s identKey

-- | Gets the display name for a user if available.
displayName :: RequestReader m => m (Maybe String)
displayName = do
    s <- session
    return $ listToMaybe $ s displayNameKey

-- | Gets the identifier for a user. If user is not logged in, redirects them
-- to the login page.
authIdentifier :: YesodAuth master => GHandler sub master String
authIdentifier = maybeIdentifier >>= maybe redirectLogin return

-- | Redirect the user to the 'defaultLoginPath', setting the DEST cookie
-- appropriately.
redirectLogin :: YesodAuth master => GHandler sub master a
redirectLogin = do
    y <- getYesod
    setUltDest'
    redirect RedirectTemporary $ defaultLoginRoute y

identKey :: String
identKey = "IDENTIFIER"

displayNameKey :: String
displayNameKey = "DISPLAY_NAME"
