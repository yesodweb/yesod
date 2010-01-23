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
    , authIdentifier
    ) where

import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Yesod
import Yesod.Constants

import Control.Applicative ((<$>))
import Control.Monad.Attempt

import Data.Maybe (fromMaybe)

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
      (\err -> redirect $ "/auth/openid/?message=" ++ encodeUrl (show err))
      redirect
      res

authOpenidComplete :: Handler y HtmlObject
authOpenidComplete = do
    gets' <- rawGetParams <$> getRawRequest
    dest <- runRequest $ cookieParam "DEST"
    res <- runAttemptT $ OpenId.authenticate gets'
    let onFailure err = redirect $ "/auth/openid/?message="
                            ++ encodeUrl (show err)
    let onSuccess (OpenId.Identifier ident) = do
        deleteCookie "DEST"
        header authCookieName ident
        redirect $ fromMaybe "/" dest
    attempt onFailure onSuccess res

rpxnowLogin :: YesodAuth y => Handler y HtmlObject
rpxnowLogin = do
    ay <- getYesod
    apiKey <- case rpxnowApiKey ay of
                Just x -> return x
                Nothing -> notFound
    token <- runRequest $ anyParam "token"
    postDest <- runRequest $ postParam "dest"
    dest' <- case postDest of
                Nothing -> runRequest $ getParam "dest"
                Just d -> return d
    let dest = case dest' of
                Nothing -> "/"
                Just "" -> "/"
                Just ('#':rest) -> rest
                Just s -> s
    ident <- Rpxnow.authenticate apiKey token
    header authCookieName $ Rpxnow.identifier ident
    header authDisplayName $ getDisplayName ident
    redirect dest

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
    ident <- identifier
    dn <- displayName
    return $ toHtmlObject
        [ ("identifier", fromMaybe "" ident)
        , ("displayName", fromMaybe "" dn)
        ]

authLogout :: YesodAuth y => Handler y HtmlObject
authLogout = do
    deleteCookie authCookieName
    ar <- getApproot
    redirect ar
    -- FIXME check the DEST information

authIdentifier :: YesodAuth y => Handler y String
authIdentifier = do
    mi <- identifier
    ar <- getApproot
    case mi of
        Nothing -> do
            rp <- requestPath
            let dest = ar ++ rp
            lp <- defaultLoginPath `fmap` getYesod
            addCookie 120 "DEST" dest
            redirect $ ar ++ lp
        Just x -> return x
