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
    ( redirectLogin
    , Auth (..)
    , AuthRoutes (..)
    , siteAuth
    , YesodAuth (..)
    , identKey
    , displayNameKey
    , Creds (..)
    , maybeCreds
    , requireCreds
    ) where

import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Yesod

import Control.Monad.Attempt
import Data.Maybe
import Control.Monad

class Yesod master => YesodAuth master where
    -- | Default destination on successful login or logout, if no other
    -- destination exists.
    defaultDest :: master -> Routes master

    -- | Default page to redirect user to for logging in.
    defaultLoginRoute :: master -> Routes master

    -- | Callback for a successful login.
    --
    -- The second parameter can contain various information, depending on login
    -- mechanism.
    onLogin :: Creds -> [(String, String)] -> GHandler Auth master ()
    onLogin _ _ = return ()

data Auth = Auth
    { authIsOpenIdEnabled :: Bool
    , authRpxnowApiKey :: Maybe String
    }

data AuthType = AuthOpenId | AuthRpxnow
    deriving (Show, Read, Eq)

-- | User credentials
data Creds = Creds
    { credsIdent :: String -- ^ Identifier. Exact meaning depends on 'credsAuthType'.
    , credsAuthType :: AuthType -- ^ How the user was authenticated
    , credsEmail :: Maybe String -- ^ Verified e-mail address.
    , credsDisplayName :: Maybe String -- ^ Display name.
    }
    deriving (Show, Read, Eq)

credsKey :: String
credsKey = "_CREDS"

setCreds :: YesodAuth master
         => Creds -> [(String, String)] -> GHandler Auth master ()
setCreds creds extra = do
    setSession credsKey $ show creds
    onLogin creds extra

maybeCreds :: GHandler sub master (Maybe Creds)
maybeCreds = do
    mcs <- lookupSession credsKey
    return $ mcs >>= readMay
  where
    readMay x = case reads x of
                    (y, _):_ -> Just y
                    _ -> Nothing

mkYesodSub "Auth" [''YesodAuth] [$parseRoutes|
/check                 Check              GET
/logout                Logout             GET
/openid                OpenIdR            GET
/openid/forward        OpenIdForward      GET
/openid/complete       OpenIdComplete     GET
/login/rpxnow          RpxnowR
|]

testOpenId :: GHandler Auth master ()
testOpenId = do
    a <- getYesodSub
    unless (authIsOpenIdEnabled a) notFound

getOpenIdR :: Yesod master => GHandler Auth master RepHtml
getOpenIdR = do
    testOpenId
    rr <- getRequest
    case getParams rr "dest" of
        [] -> return ()
        (x:_) -> setUltDestString x
    rtom <- getRouteToMaster
    message <- getMessage
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
    testOpenId
    rr <- getRequest
    oid <- case getParams rr "openid" of
            [x] -> return x
            _ -> invalidArgs [("openid", "Expected single parameter")]
    render <- getUrlRender
    toMaster <- getRouteToMaster
    let complete = render $ toMaster OpenIdComplete
    res <- runAttemptT $ OpenId.getForwardUrl oid complete
    attempt
      (\err -> do
            setMessage $ cs $ show err
            redirect RedirectTemporary $ toMaster OpenIdR)
      (redirectString RedirectTemporary)
      res

getOpenIdComplete :: YesodAuth master => GHandler Auth master ()
getOpenIdComplete = do
    testOpenId
    rr <- getRequest
    let gets' = reqGetParams rr
    res <- runAttemptT $ OpenId.authenticate gets'
    toMaster <- getRouteToMaster
    let onFailure err = do
        setMessage $ cs $ show err
        redirect RedirectTemporary $ toMaster OpenIdR
    let onSuccess (OpenId.Identifier ident) = do
        y <- getYesod
        setCreds (Creds ident AuthOpenId Nothing Nothing) []
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
                    [] -> invalidArgs [("token", "Value not supplied")]
                    (x:_) -> x
    Rpxnow.Identifier ident extra <- liftIO $ Rpxnow.authenticate apiKey token
    let creds = Creds
                    ident
                    AuthRpxnow
                    (lookup "verifiedEmail" extra)
                    (getDisplayName extra)
    setCreds creds extra
    either (redirect RedirectTemporary) (redirectString RedirectTemporary) $
        case pp "dest" of
            (d:_) -> Right d
            [] -> case getParams rr "dest" of
                    [] -> Left $ defaultDest ay
                    ("":_) -> Left $ defaultDest ay
                    (('#':rest):_) -> Right rest
                    (s:_) -> Right s

-- | Get some form of a display name.
getDisplayName :: [(String, String)] -> Maybe String
getDisplayName extra = helper choices where
    choices = ["verifiedEmail", "email", "displayName", "preferredUsername"]
    helper [] = Nothing
    helper (x:xs) = maybe (helper xs) Just $ lookup x extra

getCheck :: Yesod master => GHandler Auth master RepHtmlJson
getCheck = do
    creds <- maybeCreds
    applyLayoutJson "Authentication Status"
        (return ()) (html creds) (json creds)
  where
    html creds = [$hamlet|
%h1 Authentication Status
$if isNothing.creds
    %p Not logged in
$maybe creds c
    %p Logged in as $cs.credsIdent.c$
|]
    json creds =
        jsonMap
            [ ("ident", jsonScalar $ maybe (cs "") (cs . credsIdent) creds)
            , ("displayName", jsonScalar $ cs $ fromMaybe ""
                                         $ creds >>= credsDisplayName)
            ]

getLogout :: YesodAuth master => GHandler Auth master ()
getLogout = do
    y <- getYesod
    clearSession credsKey
    redirectUltDest RedirectTemporary $ defaultDest y

-- | Redirect the user to the 'defaultLoginPath', setting the DEST cookie
-- appropriately.
redirectLogin :: YesodAuth master => GHandler sub master a
redirectLogin = do
    y <- getYesod
    setUltDest'
    redirect RedirectTemporary $ defaultLoginRoute y

requireCreds :: YesodAuth master => GHandler sub master Creds
requireCreds = maybeCreds >>= maybe redirectLogin return

identKey :: String
identKey = "IDENTIFIER"

displayNameKey :: String
displayNameKey = "DISPLAY_NAME"
