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
    , siteAuth
    ) where

import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Yesod
import Data.Convertible.Text

import Control.Monad.Attempt
import Data.Maybe

import Data.Typeable (Typeable)
import Control.Exception (Exception)

-- FIXME check referer header to determine destination

data LoginType = OpenId | Rpxnow

data Auth = Auth
    { defaultDest :: String
    , onRpxnowLogin :: forall master. Yesod master
                    => Rpxnow.Identifier -> GHandler Auth master ()
    , rpxnowApiKey :: Maybe String
    , defaultLoginType :: LoginType
    }

$(mkYesodSub "Auth" [''Yesod] [$parseRoutes|
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
        (x:_) -> addCookie destCookieTimeout destCookieName x
    rtom <- getRouteToMaster
    let html = template (getParams rr "message", rtom)
    applyLayout "Log in via OpenID" html
  where
    urlForward (_, wrapper) = wrapper OpenIdForward
    hasMessage = not . null . fst
    message ([], _) = cs ""
    message (m:_, _) = cs m
    template = [$hamlet|
$if hasMessage
    %p.message $message$
%form!method=get!action=@urlForward@
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
    let complete = render OpenIdComplete
    res <- runAttemptT $ OpenId.getForwardUrl oid complete
    let errurl err = render OpenIdR ++ "?message=" ++ encodeUrl (show err)
    attempt
      (\err -> redirectString RedirectTemporary $ errurl err)
      (redirectString RedirectTemporary)
      res

getOpenIdComplete :: GHandler Auth master ()
getOpenIdComplete = do
    rr <- getRequest
    let gets' = reqGetParams rr
    res <- runAttemptT $ OpenId.authenticate gets'
    render <- getUrlRender
    let errurl err = render OpenIdR ++ "?message=" ++ encodeUrl (show err)
    let onFailure err = redirectString RedirectTemporary $ errurl err
    let onSuccess (OpenId.Identifier ident) = do
        y <- getYesod
        header authCookieName ident
        redirectToDest RedirectTemporary $ defaultDest y
    attempt onFailure onSuccess res

handleRpxnowR :: Yesod master => GHandler Auth master ()
handleRpxnowR = do
    ay <- getYesod
    apiKey <- case rpxnowApiKey ay of
                Just x -> return x
                Nothing -> notFound
    rr <- getRequest
    pp <- postParams rr
    let token = case getParams rr "token" ++ pp "token" of
                    [] -> failure MissingToken
                    (x:_) -> x
    let dest = case pp "dest" of
                [] -> case getParams rr "dest" of
                        [] -> defaultDest ay
                        ("":_) -> defaultDest ay
                        (('#':rest):_) -> rest
                        (s:_) -> s
                (d:_) -> d
    ident <- liftIO $ Rpxnow.authenticate apiKey token
    auth <- getYesod
    onRpxnowLogin auth ident
    header authCookieName $ Rpxnow.identifier ident
    header authDisplayName $ getDisplayName ident
    redirectToDest RedirectTemporary dest

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
    applyLayoutJson "Authentication Status" arg html json
  where
    html = [$hamlet|
%h1 Authentication Status
%dl
    %dt identifier
    %dd $fst$
    %dt displayName
    %dd $snd$
|]
    json (ident, dn) =
        jsonMap [ ("ident", jsonScalar ident)
                , ("displayName", jsonScalar dn)
                ]

getLogout :: GHandler Auth master ()
getLogout = do
    y <- getYesod
    deleteCookie authCookieName
    redirectToDest RedirectTemporary $ defaultDest y

-- | Gets the identifier for a user if available.
maybeIdentifier :: RequestReader m => m (Maybe String)
maybeIdentifier = do
    s <- session
    return $ listToMaybe $ s authCookieName

-- | Gets the display name for a user if available.
displayName :: RequestReader m => m (Maybe String)
displayName = do
    s <- session
    return $ listToMaybe $ s authDisplayName

-- | Gets the identifier for a user. If user is not logged in, redirects them
-- to the login page.
authIdentifier :: GHandler Auth master String
authIdentifier = maybeIdentifier >>= maybe redirectLogin return

-- | Redirect the user to the 'defaultLoginPath', setting the DEST cookie
-- appropriately.
redirectLogin :: GHandler Auth master a
redirectLogin = do
    y <- getYesod
    let r = case defaultLoginType y of
                OpenId -> OpenIdR
                Rpxnow -> RpxnowR -- FIXME this doesn't actually show a login page?
    redirectSetDest RedirectTemporary r

-- | Redirect to the given URL, and set a cookie with the current URL so the
-- user will ultimately be sent back here.
redirectSetDest :: RedirectType
                -> Routes sub -- ^ redirect page
                -> GHandler sub master a
redirectSetDest rt dest = do
    ur <- getUrlRender
    toMaster <- getRouteToMaster
    curr <- getRoute
    let curr' = case curr of
                    Just x -> ur x
                    Nothing -> "/" -- should never happen anyway
    addCookie destCookieTimeout destCookieName curr'
    redirect rt $ toMaster dest

-- | Read the 'destCookieName' cookie and redirect to this destination. If the
-- cookie is missing, then use the default path provided.
redirectToDest :: RedirectType -> String -> GHandler sub master a
redirectToDest rt def = do
    rr <- getRequest
    dest <- case cookies rr destCookieName of
                [] -> return def
                (x:_) -> do
                    deleteCookie destCookieName
                    return x
    redirectString rt dest
