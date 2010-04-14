{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
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
    , redirectLogin
    ) where

-- FIXME write as subsite

import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Yesod
import Data.Convertible.Text

import Control.Monad.Attempt
import qualified Data.ByteString.Char8 as B8

import qualified Network.Wai as W
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Applicative ((<$>))

-- FIXME check referer header to determine destination

class Yesod a => YesodAuth a where
    -- | The following breaks DRY, but I cannot think of a better solution
    -- right now.
    --
    -- The root relative to the application root. Should not begin with a slash
    -- and should end with one.
    authRoot :: a -> String
    authRoot _ = "auth/"

    -- | Absolute path to the default login path.
    defaultLoginPath :: a -> String
    defaultLoginPath a = approot a ++ authRoot a ++ "openid/"

    rpxnowApiKey :: a -> Maybe String
    rpxnowApiKey _ = Nothing

    onRpxnowLogin :: Rpxnow.Identifier -> Handler a ()
    onRpxnowLogin _ = return ()

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

authHandler :: YesodAuth y => W.Method -> [String] -> Handler y ChooseRep
authHandler W.GET ["check"] = rc authCheck
authHandler W.GET ["logout"] = rc authLogout
authHandler W.GET ["openid"] = rc authOpenidForm
authHandler W.GET ["openid", "forward"] = rc authOpenidForward
authHandler W.GET ["openid", "complete"] = rc authOpenidComplete
-- two different versions of RPX protocol apparently, so just accepting all
-- verbs
authHandler _ ["login", "rpxnow"] = rc rpxnowLogin
authHandler _ _ = notFound

-- FIXME data OIDFormReq = OIDFormReq (Maybe String) (Maybe String)
{- FIXME
instance ConvertSuccess OIDFormReq Html where
    convertSuccess (OIDFormReq Nothing _) = cs ""
    convertSuccess (OIDFormReq (Just s) _) =
        Tag "p" [("class", "message")] $ cs s
-}

data ExpectedSingleParam = ExpectedSingleParam
    deriving (Show, Typeable)
instance Exception ExpectedSingleParam

authOpenidForm :: Yesod y => Handler y ChooseRep
authOpenidForm = do
    rr <- getRequest
    case getParams rr "dest" of
        [] -> return ()
        (x:_) -> addCookie destCookieTimeout destCookieName x
    let html = template (getParams rr "message")
    simpleApplyLayout "Log in via OpenID" html
  where
    urlForward _ = error "FIXME urlForward"
    hasMessage = return . not . null
    message [] = return $ Encoded $ cs ""
    message (m:_) = return $ Unencoded $ cs m
    template = [$hamlet|
$if hasMessage
    %p.message $message$
%form!method=get!action=@urlForward@
    %label!for=openid OpenID: 
    %input#openid!type=text!name=openid
    %input!type=submit!value=Login
|]

authOpenidForward :: YesodAuth y => Handler y ()
authOpenidForward = do
    rr <- getRequest
    oid <- case getParams rr "openid" of
            [x] -> return x
            _ -> invalidArgs [("openid", show ExpectedSingleParam)]
    authroot <- getFullAuthRoot
    let complete = authroot ++ "/openid/complete/"
    res <- runAttemptT $ OpenId.getForwardUrl oid complete
    attempt
      (\err -> redirect RedirectTemporary
                  $ "/auth/openid/?message=" ++ encodeUrl (show err))
      (redirect RedirectTemporary)
      res

authOpenidComplete :: Yesod y => Handler y ()
authOpenidComplete = do
    rr <- getRequest
    let gets' = reqGetParams rr
    res <- runAttemptT $ OpenId.authenticate gets'
    let onFailure err = redirect RedirectTemporary
                             $ "/auth/openid/?message="
                            ++ encodeUrl (show err)
    let onSuccess (OpenId.Identifier ident) = do
        ar <- getApproot
        header authCookieName ident
        redirectToDest RedirectTemporary ar
    attempt onFailure onSuccess res

rpxnowLogin :: YesodAuth y => Handler y ()
rpxnowLogin = do
    ay <- getYesod
    let ar = approot ay
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
                        [] -> ar
                        ("":_) -> ar
                        (('#':rest):_) -> rest
                        (s:_) -> s
                (d:_) -> d
    ident <- liftIO $ Rpxnow.authenticate apiKey token
    onRpxnowLogin ident
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

authCheck :: Yesod y => Handler y ChooseRep
authCheck = do
    _ident <- maybeIdentifier
    _dn <- displayName
    error "FIXME applyLayoutJson"
    {-
    applyLayoutJson "Authentication Status" $ cs
        [ ("identifier", fromMaybe "" ident)
        , ("displayName", fromMaybe "" dn)
        ]
    -}

authLogout :: YesodAuth y => Handler y ()
authLogout = do
    deleteCookie authCookieName
    getApproot >>= redirectToDest RedirectTemporary

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
authIdentifier :: YesodAuth y => Handler y String
authIdentifier = maybeIdentifier >>= maybe redirectLogin return

-- | Redirect the user to the 'defaultLoginPath', setting the DEST cookie
-- appropriately.
redirectLogin :: YesodAuth y => Handler y a
redirectLogin =
    defaultLoginPath `fmap` getYesod >>= redirectSetDest RedirectTemporary

-- | Determinge the path requested by the user (ie, the path info). This
-- includes the query string.
requestPath :: (Functor m, Monad m, RequestReader m) => m String
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
redirectSetDest :: Yesod y => RedirectType -> String -> Handler y a
redirectSetDest rt dest = do
    ar <- getApproot
    rp <- requestPath
    let curr = ar ++ rp
    addCookie destCookieTimeout destCookieName curr
    redirect rt dest

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
