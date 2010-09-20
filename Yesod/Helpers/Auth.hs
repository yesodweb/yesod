{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ( -- * Subsite
      Auth (..)
    , getAuth
    , AuthRoute (..)
      -- * Settings
    , YesodAuth (..)
    , Creds (..)
    , EmailCreds (..)
    , AuthType (..)
    , RpxnowSettings (..)
    , EmailSettings (..)
    , FacebookSettings (..)
    , getFacebookUrl
      -- * Functions
    , maybeAuth
    , maybeAuthId
    , requireAuth
    , requireAuthId
    ) where

import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId
import qualified Web.Authenticate.Facebook as Facebook

import Yesod
import Yesod.Mail (randomString)

import Data.Maybe
import Data.Int (Int64)
import Control.Monad
import System.Random
import Data.Digest.Pure.MD5
import Control.Applicative
import Control.Monad.Attempt
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Object
import Language.Haskell.TH.Syntax

type AuthId m = Key (AuthEntity m)
type AuthEmailId m = Key (AuthEmailEntity m)

class ( Yesod master
      , PersistEntity (AuthEntity master)
      , PersistEntity (AuthEmailEntity master)
      ) => YesodAuth master where
    type AuthEntity master
    type AuthEmailEntity master

    -- | Default destination on successful login or logout, if no other
    -- destination exists.
    defaultDest :: master -> Route master

    getAuthId :: Creds master -> [(String, String)]
              -> GHandler s master (Maybe (AuthId master))

    -- | Generate a random alphanumeric string.
    --
    -- This is used for verify string in email authentication.
    randomKey :: master -> IO String
    randomKey _ = do
        stdgen <- newStdGen
        return $ fst $ randomString 10 stdgen

    openIdEnabled :: master -> Bool
    openIdEnabled _ = False

    rpxnowSettings :: master -> Maybe RpxnowSettings
    rpxnowSettings _ = Nothing

    emailSettings :: master -> Maybe (EmailSettings master)
    emailSettings _ = Nothing

    -- | client id, secret and requested permissions
    facebookSettings :: master -> Maybe FacebookSettings
    facebookSettings _ = Nothing

data Auth = Auth

getAuth :: a -> Auth
getAuth = const Auth

-- | Which subsystem authenticated the user.
data AuthType = AuthOpenId | AuthRpxnow | AuthEmail | AuthFacebook
    deriving (Show, Read, Eq)

type Email = String
type VerKey = String
type VerUrl = String
type SaltedPass = String
type VerStatus = Bool

-- | Data stored in a database for each e-mail address.
data EmailCreds m = EmailCreds
    { emailCredsId :: AuthEmailId m
    , emailCredsAuthId :: Maybe (AuthId m)
    , emailCredsStatus :: VerStatus
    , emailCredsVerkey :: Maybe VerKey
    }

data RpxnowSettings = RpxnowSettings
    { rpxnowApp :: String
    , rpxnowKey :: String
    }

data EmailSettings m = EmailSettings
    { addUnverified :: Email -> VerKey -> GHandler Auth m (AuthEmailId m)
    , sendVerifyEmail :: Email -> VerKey -> VerUrl -> GHandler Auth m ()
    , getVerifyKey :: AuthEmailId m -> GHandler Auth m (Maybe VerKey)
    , setVerifyKey :: AuthEmailId m -> VerKey -> GHandler Auth m ()
    , verifyAccount :: AuthEmailId m -> GHandler Auth m (Maybe (AuthId m))
    , getPassword :: AuthId m -> GHandler Auth m (Maybe SaltedPass)
    , setPassword :: AuthId m -> SaltedPass -> GHandler Auth m ()
    , getEmailCreds :: Email -> GHandler Auth m (Maybe (EmailCreds m))
    , getEmail :: AuthEmailId m -> GHandler Auth m (Maybe Email)
    }

data FacebookSettings = FacebookSettings
    { fbAppId :: String
    , fbSecret :: String
    , fbPerms :: [String]
    }

-- | User credentials
data Creds m = Creds
    { credsIdent :: String -- ^ Identifier. Exact meaning depends on 'credsAuthType'.
    , credsAuthType :: AuthType -- ^ How the user was authenticated
    , credsEmail :: Maybe String -- ^ Verified e-mail address.
    , credsDisplayName :: Maybe String -- ^ Display name.
    , credsId :: Maybe (AuthId m) -- ^ Numeric ID, if used.
    , credsFacebookToken :: Maybe Facebook.AccessToken
    }

credsKey :: String
credsKey = "_ID"

setCreds :: YesodAuth master
         => Creds master -> [(String, String)] -> GHandler Auth master ()
setCreds creds extra = do
    maid <- getAuthId creds extra
    case maid of
        Nothing -> return ()
        Just aid -> setSession credsKey $ show $ fromPersistKey aid

-- | Retrieves user credentials, if user is authenticated.
maybeAuthId :: YesodAuth m => GHandler s m (Maybe (AuthId m))
maybeAuthId = do
    ms <- lookupSession credsKey
    case ms of
        Nothing -> return Nothing
        Just s -> case reads s of
                    [] -> return Nothing
                    (i, _):_ -> return $ Just $ toPersistKey i

maybeAuth :: ( PersistBackend (YesodDB m (GHandler s m))
             , YesodPersist m
             , YesodAuth m
             ) => GHandler s m (Maybe (AuthId m, AuthEntity m))
maybeAuth = do
    maid <- maybeAuthId
    case maid of
        Nothing -> return Nothing
        Just aid -> do
            ma <- runDB $ get aid
            case ma of
                Nothing -> return Nothing
                Just a -> return $ Just (aid, a)

mkYesodSub "Auth"
    [ ClassP ''YesodAuth [VarT $ mkName "master"]
    ]
    [$parseRoutes|
/check                   CheckR             GET
/logout                  LogoutR            GET
/openid/forward          OpenIdForwardR     GET
/openid/complete         OpenIdCompleteR    GET
/login/rpxnow            RpxnowR

/facebook                FacebookR          GET

/register                EmailRegisterR     GET POST
/verify/#Int64/#String   EmailVerifyR       GET
/email-login             EmailLoginR        POST
/set-password            EmailPasswordR     GET POST

/login                   LoginR             GET
|]

testOpenId :: YesodAuth master => GHandler Auth master ()
testOpenId = do
    a <- getYesod
    unless (openIdEnabled a) notFound

getOpenIdForwardR :: YesodAuth master => GHandler Auth master ()
getOpenIdForwardR = do
    testOpenId
    oid <- runFormGet' $ stringInput "openid"
    render <- getUrlRender
    toMaster <- getRouteToMaster
    let complete = render $ toMaster OpenIdCompleteR
    res <- runAttemptT $ OpenId.getForwardUrl oid complete
    attempt
      (\err -> do
            setMessage $ string $ show err
            redirect RedirectTemporary $ toMaster LoginR)
      (redirectString RedirectTemporary)
      res

getOpenIdCompleteR :: YesodAuth master => GHandler Auth master ()
getOpenIdCompleteR = do
    testOpenId
    rr <- getRequest
    let gets' = reqGetParams rr
    res <- runAttemptT $ OpenId.authenticate gets'
    toMaster <- getRouteToMaster
    let onFailure err = do
        setMessage $ string $ show err
        redirect RedirectTemporary $ toMaster LoginR
    let onSuccess (OpenId.Identifier ident) = do
        y <- getYesod
        setCreds (Creds ident AuthOpenId Nothing Nothing Nothing Nothing) []
        redirectUltDest RedirectTemporary $ defaultDest y
    attempt onFailure onSuccess res

handleRpxnowR :: YesodAuth master => GHandler Auth master ()
handleRpxnowR = do
    ay <- getYesod
    auth <- getYesod
    apiKey <- case rpxnowKey <$> rpxnowSettings auth of
                Just x -> return x
                Nothing -> notFound
    token1 <- lookupGetParam "token"
    token2 <- lookupPostParam "token"
    let token = case token1 `mplus` token2 of
                    Nothing -> invalidArgs ["token: Value not supplied"]
                    Just x -> x
    Rpxnow.Identifier ident extra <- liftIO $ Rpxnow.authenticate apiKey token
    let creds = Creds
                    ident
                    AuthRpxnow
                    (lookup "verifiedEmail" extra)
                    (getDisplayName extra)
                    Nothing
                    Nothing
    setCreds creds extra
    dest1 <- lookupPostParam "dest"
    dest2 <- lookupGetParam "dest"
    either (redirect RedirectTemporary) (redirectString RedirectTemporary) $
        case dest1 `mplus` dest2 of
            Just "" -> Left $ defaultDest ay
            Nothing -> Left $ defaultDest ay
            Just ('#':d) -> Right d
            Just d -> Right d

-- | Get some form of a display name.
getDisplayName :: [(String, String)] -> Maybe String
getDisplayName extra =
    foldr (\x -> mplus (lookup x extra)) Nothing choices
  where
    choices = ["verifiedEmail", "email", "displayName", "preferredUsername"]

getCheckR :: YesodAuth master => GHandler Auth master RepHtmlJson
getCheckR = do
    creds <- maybeAuthId
    defaultLayoutJson (do
        setTitle "Authentication Status"
        addBody $ html creds) (json creds)
  where
    html creds = [$hamlet|
%h1 Authentication Status
$if isNothing.creds
    %p Not logged in.
$maybe creds _
    %p Logged in.
|]
    json creds =
        jsonMap
            [ ("logged_in", jsonScalar $ maybe "false" (const "true") creds)
            ]

getLogoutR :: YesodAuth master => GHandler Auth master ()
getLogoutR = do
    y <- getYesod
    deleteSession credsKey
    redirectUltDest RedirectTemporary $ defaultDest y

-- | Retrieve user credentials. If user is not logged in, redirects to the
-- 'authRoute'. Sets ultimate destination to current route, so user
-- should be sent back here after authenticating.
requireAuthId :: YesodAuth m => GHandler sub m (AuthId m)
requireAuthId = maybeAuthId >>= maybe redirectLogin return

requireAuth :: ( PersistBackend (YesodDB m (GHandler s m))
               , YesodPersist m
               , YesodAuth m
               ) => GHandler s m (AuthId m, AuthEntity m)
requireAuth = maybeAuth >>= maybe redirectLogin return

redirectLogin :: Yesod m => GHandler s m a
redirectLogin = do
    y <- getYesod
    setUltDest'
    case authRoute y of
        Just z -> redirect RedirectTemporary z
        Nothing -> permissionDenied "Please configure authRoute"

getEmailSettings :: YesodAuth master
                     => GHandler Auth master (EmailSettings master)
getEmailSettings = getYesod >>= maybe notFound return . emailSettings

getEmailRegisterR :: YesodAuth master => GHandler Auth master RepHtml
getEmailRegisterR = do
    _ae <- getEmailSettings
    toMaster <- getRouteToMaster
    defaultLayout $ setTitle "Register a new account" >> addBody [$hamlet|
%p Enter your e-mail address below, and a confirmation e-mail will be sent to you.
%form!method=post!action=@toMaster.EmailRegisterR@
    %label!for=email E-mail
    %input#email!type=email!name=email!width=150
    %input!type=submit!value=Register
|]

postEmailRegisterR :: YesodAuth master => GHandler Auth master RepHtml
postEmailRegisterR = do
    ae <- getEmailSettings
    email <- runFormPost' $ emailInput "email"
    mecreds <- getEmailCreds ae email
    (lid, verKey) <-
        case mecreds of
            Just (EmailCreds lid _ _ (Just key)) -> return (lid, key)
            Just (EmailCreds lid _ _ Nothing) -> do
                y <- getYesod
                key <- liftIO $ randomKey y
                setVerifyKey ae lid key
                return (lid, key)
            Nothing -> do
                y <- getYesod
                key <- liftIO $ randomKey y
                lid <- addUnverified ae email key
                return (lid, key)
    render <- getUrlRender
    tm <- getRouteToMaster
    let verUrl = render $ tm $ EmailVerifyR (fromPersistKey lid) verKey
    sendVerifyEmail ae email verKey verUrl
    defaultLayout $ setTitle "Confirmation e-mail sent" >> addBody [$hamlet|
%p A confirmation e-mail has been sent to $email$.
|]

getEmailVerifyR :: YesodAuth master
           => Int64 -> String -> GHandler Auth master RepHtml
getEmailVerifyR lid' key = do
    let lid = toPersistKey lid'
    ae <- getEmailSettings
    realKey <- getVerifyKey ae lid
    memail <- getEmail ae lid
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            muid <- verifyAccount ae lid
            case muid of
                Nothing -> return ()
                Just uid -> do
                    setCreds (Creds email AuthEmail (Just email) Nothing (Just uid)
                              Nothing) []
                    toMaster <- getRouteToMaster
                    redirect RedirectTemporary $ toMaster EmailPasswordR
        _ -> return ()
    defaultLayout $ do
        setTitle "Invalid verification key"
        addBody [$hamlet|
%p I'm sorry, but that was an invalid verification key.
|]

postEmailLoginR :: YesodAuth master => GHandler Auth master ()
postEmailLoginR = do
    ae <- getEmailSettings
    (email, pass) <- runFormPost' $ (,)
        <$> emailInput "email"
        <*> stringInput "password"
    y <- getYesod
    mecreds <- getEmailCreds ae email
    maid <-
        case (mecreds >>= emailCredsAuthId, fmap emailCredsStatus mecreds) of
            (Just aid, Just True) -> do
                mrealpass <- getPassword ae aid
                case mrealpass of
                    Nothing -> return Nothing
                    Just realpass -> return $
                        if isValidPass pass realpass
                            then Just aid
                            else Nothing
            _ -> return Nothing
    case maid of
        Just aid -> do
            setCreds (Creds email AuthEmail (Just email) Nothing (Just aid)
                      Nothing) []
            redirectUltDest RedirectTemporary $ defaultDest y
        Nothing -> do
            setMessage $ string "Invalid email/password combination"
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

getEmailPasswordR :: YesodAuth master => GHandler Auth master RepHtml
getEmailPasswordR = do
    _ae <- getEmailSettings
    toMaster <- getRouteToMaster
    maid <- maybeAuthId
    case maid of
        Just _ -> return ()
        Nothing -> do
            setMessage $ string "You must be logged in to set a password"
            redirect RedirectTemporary $ toMaster EmailLoginR
    defaultLayout $ do
        setTitle "Set password"
        addBody [$hamlet|
%h3 Set a new password
%form!method=post!action=@toMaster.EmailPasswordR@
    %table
        %tr
            %th New password
            %td
                %input!type=password!name=new
        %tr
            %th Confirm
            %td
                %input!type=password!name=confirm
        %tr
            %td!colspan=2
                %input!type=submit!value=Submit
|]

postEmailPasswordR :: YesodAuth master => GHandler Auth master ()
postEmailPasswordR = do
    ae <- getEmailSettings
    (new, confirm) <- runFormPost' $ (,)
        <$> stringInput "new"
        <*> stringInput "confirm"
    toMaster <- getRouteToMaster
    when (new /= confirm) $ do
        setMessage $ string "Passwords did not match, please try again"
        redirect RedirectTemporary $ toMaster EmailPasswordR
    maid <- maybeAuthId
    aid <- case maid of
            Nothing -> do
                setMessage $ string "You must be logged in to set a password"
                redirect RedirectTemporary $ toMaster EmailLoginR
            Just aid -> return aid
    salted <- liftIO $ saltPass new
    setPassword ae aid salted
    setMessage $ string "Password updated"
    y <- getYesod
    redirect RedirectTemporary $ defaultDest y

saltLength :: Int
saltLength = 5

isValidPass :: String -- ^ cleartext password
            -> String -- ^ salted password
            -> Bool
isValidPass clear salted =
    let salt = take saltLength salted
     in salted == saltPass' salt clear

saltPass :: String -> IO String
saltPass pass = do
    stdgen <- newStdGen
    let salt = take saltLength $ randomRs ('A', 'Z') stdgen
    return $ saltPass' salt pass

saltPass' :: String -> String -> String
saltPass' salt pass = salt ++ show (md5 $ fromString $ salt ++ pass)

getFacebookR :: YesodAuth master => GHandler Auth master ()
getFacebookR = do
    y <- getYesod
    a <- facebookSettings <$> getYesod
    case a of
        Nothing -> notFound
        Just (FacebookSettings cid secret _) -> do
            render <- getUrlRender
            tm <- getRouteToMaster
            let fb = Facebook.Facebook cid secret $ render $ tm FacebookR
            code <- runFormGet' $ stringInput "code"
            at <- liftIO $ Facebook.getAccessToken fb code
            so <- liftIO $ Facebook.getGraphData at "me"
            let c = fromMaybe (error "Invalid response from Facebook") $ do
                m <- fromMapping so
                id' <- lookupScalar "id" m
                let name = lookupScalar "name" m
                let email = lookupScalar "email" m
                let id'' = "http://graph.facebook.com/" ++ id'
                return $ Creds id'' AuthFacebook email name Nothing $ Just at
            setCreds c []
            redirectUltDest RedirectTemporary $ defaultDest y

getFacebookUrl :: YesodAuth m
               => (AuthRoute -> Route m) -> GHandler s m (Maybe String)
getFacebookUrl tm = do
    y <- getYesod
    render <- getUrlRender
    case facebookSettings y of
        Nothing -> return Nothing
        Just f -> do
            let fb =
                    Facebook.Facebook
                        (fbAppId f)
                        (fbSecret f)
                        (render $ tm FacebookR)
            return $ Just $ Facebook.getForwardUrl fb $ fbPerms f

getLoginR :: YesodAuth master => GHandler Auth master RepHtml
getLoginR = do
    lookupGetParam "dest" >>= maybe (return ()) setUltDestString
    tm <- getRouteToMaster
    y <- getYesod
    fb <- getFacebookUrl tm
    defaultLayout $ do
        setTitle "Login"
        addStyle [$cassius|
#openid
    background: #fff url(http://www.myopenid.com/static/openid-icon-small.gif) no-repeat scroll 0pt 50%;
    padding-left: 18px;
|]
        addBody [$hamlet|
$maybe emailSettings.y _
    %h3 Email
    %form!method=post!action=@tm.EmailLoginR@
        %table
            %tr
                %th E-mail
                %td
                    %input!type=email!name=email
            %tr
                %th Password
                %td
                    %input!type=password!name=password
            %tr
                %td!colspan=2
                    %input!type=submit!value="Login via email"
                    %a!href=@tm.EmailRegisterR@ I don't have an account
$if openIdEnabled.y
    %h3 OpenID
    %form!action=@tm.OpenIdForwardR@
        %label!for=openid OpenID: $
        %input#openid!type=text!name=openid
        %input!type=submit!value="Login via OpenID"
$maybe fb f
    %h3 Facebook
    %p
        %a!href=$f$ Login via Facebook
$maybe rpxnowSettings.y r
    %h3 Rpxnow
    %p
        %a!onclick="return false;"!href="https://$rpxnowApp.r$.rpxnow.com/openid/v2/signin?token_url=@tm.RpxnowR@"
            Login via Rpxnow
|]
