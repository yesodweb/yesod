{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
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
    , siteAuthRoutes
    ) where

import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Yesod
import Data.Convertible.Text

import Control.Monad.Attempt
import qualified Data.ByteString.Char8 as B8
import Data.Maybe

import qualified Network.Wai as W
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Applicative ((<$>))

-- FIXME check referer header to determine destination

data LoginType = OpenId | Rpxnow

data Auth = forall y. Yesod y => Auth
    { defaultDest :: String
    , onRpxnowLogin :: Rpxnow.Identifier -> Handler Auth ()
    , rpxnowApiKey :: Maybe String
    , defaultLoginType :: LoginType
    , parentYesod :: y
    }

$(mkYesod "Auth" [$parseRoutes|
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

getOpenIdR :: Handler Auth RepHtml
getOpenIdR = do
    rr <- getRequest
    case getParams rr "dest" of
        [] -> return ()
        (x:_) -> addCookie destCookieTimeout destCookieName x
    (Auth _ _ _ _ y) <- getYesod
    let html = template (getParams rr "message", id)
    let pc = PageContent
                { pageTitle = cs "Log in via OpenID"
                , pageHead = return ()
                , pageBody = html
                }
    content <- hamletToContent $ applyLayout y pc rr
    return $ RepHtml content
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

getOpenIdForward :: Handler Auth ()
getOpenIdForward = do
    rr <- getRequest
    oid <- case getParams rr "openid" of
            [x] -> return x
            _ -> invalidArgs [("openid", show ExpectedSingleParam)]
    render <- getUrlRender
    let complete = render OpenIdComplete
    res <- runAttemptT $ OpenId.getForwardUrl oid complete
    attempt
      (\err -> redirect RedirectTemporary
                  $ "/auth/openid/?message=" ++ encodeUrl (show err))
      (redirect RedirectTemporary)
      res

getOpenIdComplete :: Handler Auth ()
getOpenIdComplete = do
    rr <- getRequest
    let gets' = reqGetParams rr
    res <- runAttemptT $ OpenId.authenticate gets'
    let onFailure err = redirect RedirectTemporary
                             $ "/auth/openid/?message="
                            ++ encodeUrl (show err)
    let onSuccess (OpenId.Identifier ident) = do
        y <- getYesod
        header authCookieName ident
        redirectToDest RedirectTemporary $ defaultDest y
    attempt onFailure onSuccess res

handleRpxnowR :: Handler Auth ()
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
    helper (x:xs) = case lookup x extra of
                        Nothing -> helper xs
                        Just y -> y

getCheck :: Handler Auth RepHtml
getCheck = do
    ident <- maybeIdentifier
    dn <- displayName
    -- FIXME applyLayoutJson
    hamletToRepHtml $ [$hamlet|
%h1 Authentication Status
%dl
    %dt identifier
    %dd $fst$
    %dt displayName
    %dd $snd$
|] (cs $ fromMaybe "" ident, cs $ fromMaybe "" dn)

getLogout :: Handler Auth ()
getLogout = do
    y <- getYesod
    deleteCookie authCookieName
    redirectToDest RedirectTemporary $ defaultDest y

-- | Gets the identifier for a user if available.
maybeIdentifier :: (Functor m, Monad m, RequestReader m) => m (Maybe String)
maybeIdentifier =
    fmap cs . lookup (B8.pack authCookieName) . reqSession
    <$> getRequest

-- | Gets the display name for a user if available.
displayName :: (Functor m, Monad m, RequestReader m) => m (Maybe String)
displayName = do
    rr <- getRequest
    return $ fmap cs $ lookup (B8.pack authDisplayName) $ reqSession rr

-- | Gets the identifier for a user. If user is not logged in, redirects them
-- to the login page.
authIdentifier :: Handler Auth String
authIdentifier = maybeIdentifier >>= maybe redirectLogin return

-- | Redirect the user to the 'defaultLoginPath', setting the DEST cookie
-- appropriately.
redirectLogin :: Handler Auth a
redirectLogin = do
    y <- getYesod
    let r = case defaultLoginType y of
                OpenId -> OpenIdR
                Rpxnow -> RpxnowR -- FIXME this doesn't actually show a login page?
    redirectSetDest RedirectTemporary r

-- | Determinge the path requested by the user (ie, the path info). This
-- includes the query string.
requestPath :: (Functor m, Monad m, RequestReader m) => m String --FIXME unused
requestPath = do
    env <- waiRequest
    let q = case B8.unpack $ W.queryString env of
                "" -> ""
                q'@('?':_) -> q'
                q' -> '?' : q'
    return $! dropSlash (B8.unpack $ W.pathInfo env) ++ q
      where
        dropSlash ('/':x) = x
        dropSlash x = x

-- | Redirect to the given URL, and set a cookie with the current URL so the
-- user will ultimately be sent back here.
redirectSetDest :: RedirectType
                -> Routes y -- ^ redirect page
                -> Handler y a
redirectSetDest rt dest = do
    ur <- getUrlRender
    curr <- getRoute
    let curr' = case curr of
                    Just x -> ur x
                    Nothing -> "/" -- should never happen anyway
        dest' = ur dest
    addCookie destCookieTimeout destCookieName curr'
    redirect rt dest'

-- | Read the 'destCookieName' cookie and redirect to this destination. If the
-- cookie is missing, then use the default path provided.
redirectToDest :: RedirectType -> String -> Handler y a
redirectToDest rt def = do
    rr <- getRequest
    dest <- case cookies rr destCookieName of
                [] -> return def
                (x:_) -> do
                    deleteCookie destCookieName
                    return x
    redirect rt dest
