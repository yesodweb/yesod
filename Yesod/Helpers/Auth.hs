{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    , maybeIdentifier
    , authIdentifier
    , displayName
    ) where

import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Yesod

import Control.Monad.Attempt

import Data.Maybe (fromMaybe)
import qualified Hack
import Data.Typeable (Typeable)
import Control.Exception (Exception)

class YesodApproot a => YesodAuth a where
    -- | The following breaks DRY, but I cannot think of a better solution
    -- right now.
    --
    -- The root relative to the application root. Should not begin with a slash
    -- and should end with one.
    authRoot :: a -> String
    authRoot _ = "auth/"

    defaultLoginPath :: a -> String
    defaultLoginPath a = authRoot a ++ "openid/"

    rpxnowApiKey :: a -> Maybe String
    rpxnowApiKey _ = Nothing

getFullAuthRoot :: YesodAuth y => Handler y String
getFullAuthRoot = do
    y <- getYesod
    ar <- getApproot
    return $ ar ++ authRoot y

data AuthResource =
    Check
    | Logout
    | Openid
    | OpenidForward
    | OpenidComplete
    | LoginRpxnow
    deriving (Show, Eq, Enum, Bounded)

rc :: HasReps x => Handler y x -> Handler y ChooseRep
rc = fmap chooseRep

authHandler :: YesodAuth y => Verb -> [String] -> Handler y ChooseRep
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
instance ConvertSuccess OIDFormReq Html where
    convertSuccess (OIDFormReq Nothing _) = cs ""
    convertSuccess (OIDFormReq (Just s) _) =
        Tag "p" [("class", "message")] $ cs s

authOpenidForm :: Handler y HtmlObject
authOpenidForm = do
    message <- runRequest $ getParam "message"
    dest <- runRequest $ getParam "dest"
    let m = OIDFormReq message dest
    let html =
         HtmlList
          [ cs m
          , Tag "form" [("method", "get"), ("action", "forward/")] $
              HtmlList
                [ Tag "label" [("for", "openid")] $ cs "OpenID: "
                , EmptyTag "input" [("type", "text"), ("id", "openid"),
                                    ("name", "openid")]
                , EmptyTag "input" [("type", "submit"), ("value", "Login")]
                ]
          ]
    case dest of
        Just dest' -> addCookie 120 "DEST" dest'
        Nothing -> return ()
    return $ cs html

authOpenidForward :: YesodAuth y => Handler y HtmlObject
authOpenidForward = do
    oid <- runRequest $ getParam "openid"
    authroot <- getFullAuthRoot
    let complete = authroot ++ "/openid/complete/"
    res <- runAttemptT $ OpenId.getForwardUrl oid complete
    attempt
      (\err -> redirect RedirectTemporary
                  $ "/auth/openid/?message=" ++ encodeUrl (show err))
      (redirect RedirectTemporary)
      res

authOpenidComplete :: YesodApproot y => Handler y HtmlObject
authOpenidComplete = do
    ar <- getApproot
    rr <- getRawRequest
    let gets' = rawGetParams rr
    let dest = case cookies rr "DEST" of
                [] -> ar
                (x:_) -> x
    res <- runAttemptT $ OpenId.authenticate gets'
    let onFailure err = redirect RedirectTemporary
                             $ "/auth/openid/?message="
                            ++ encodeUrl (show err)
    let onSuccess (OpenId.Identifier ident) = do
        deleteCookie "DEST"
        header authCookieName ident
        redirect RedirectTemporary dest
    attempt onFailure onSuccess res

rpxnowLogin :: YesodAuth y => Handler y HtmlObject
rpxnowLogin = do
    ay <- getYesod
    let ar = approot ay
    apiKey <- case rpxnowApiKey ay of
                Just x -> return x
                Nothing -> notFound
    rr <- getRawRequest
    let token = case getParams rr "token" ++ postParams rr "token" of
                    [] -> failure MissingToken
                    (x:_) -> x
    postDest <- runRequest $ postParam "dest"
    dest' <- case postDest of
                Nothing -> runRequest $ getParam "dest"
                Just d -> return d
    let dest = case dest' of
                Nothing -> ar
                Just "" -> ar
                Just ('#':rest) -> rest
                Just s -> s
    ident <- Rpxnow.authenticate apiKey token
    header authCookieName $ Rpxnow.identifier ident
    header authDisplayName $ getDisplayName ident
    redirect RedirectTemporary dest

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

authCheck :: Handler y HtmlObject
authCheck = do
    ident <- maybeIdentifier
    dn <- displayName
    return $ toHtmlObject
        [ ("identifier", fromMaybe "" ident)
        , ("displayName", fromMaybe "" dn)
        ]

authLogout :: YesodAuth y => Handler y HtmlObject
authLogout = do
    deleteCookie authCookieName
    ar <- getApproot
    redirect RedirectTemporary ar
    -- FIXME check the DEST information

-- | Gets the identifier for a user if available.
maybeIdentifier :: (Functor m, Monad m, RequestReader m) => m (Maybe String)
maybeIdentifier = do
    env <- parseEnv
    case lookup authCookieName $ Hack.hackHeaders env of
        Nothing -> return Nothing
        Just x -> return (Just x)

-- | Gets the display name for a user if available.
displayName :: (Functor m, Monad m, RequestReader m) => m (Maybe String)
displayName = do
    env <- parseEnv
    case lookup authDisplayName $ Hack.hackHeaders env of
        Nothing -> return Nothing
        Just x -> return (Just x)

-- | Gets the identifier for a user. If user is not logged in, redirects them
-- to the login page.
authIdentifier :: YesodAuth y => Handler y String
authIdentifier = do
    mi <- maybeIdentifier
    ar <- getApproot
    case mi of
        Nothing -> do
            rp <- requestPath
            let dest = ar ++ rp
            lp <- defaultLoginPath `fmap` getYesod
            addCookie 120 "DEST" dest
            redirect RedirectTemporary $ ar ++ lp
        Just x -> return x

-- | Determinge the path requested by the user (ie, the path info). This
-- includes the query string.
requestPath :: (Functor m, Monad m, RequestReader m) => m String
requestPath = do
    env <- parseEnv
    let q = case Hack.queryString env of
                "" -> ""
                q'@('?':_) -> q'
                q' -> '?' : q'
    return $! dropSlash (Hack.pathInfo env) ++ q
      where
        dropSlash ('/':x) = x
        dropSlash x = x
