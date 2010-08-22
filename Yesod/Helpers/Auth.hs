{-# LANGUAGE FlexibleInstances #-}
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
    , AuthRoute (..)
      -- * Settings
    , YesodAuth (..)
    , Creds (..)
    , EmailCreds (..)
    , AuthType (..)
    , AuthEmailSettings (..)
    , inMemoryEmailSettings
      -- * Functions
    , maybeCreds
    , requireCreds
    ) where

import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId
import qualified Web.Authenticate.Facebook as Facebook

import Yesod
import Yesod.Mail (randomString)

import Data.Maybe
import Control.Monad
import System.Random
import Data.Digest.Pure.MD5
import Control.Applicative
import Control.Concurrent.MVar
import System.IO
import Control.Monad.Attempt
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Object
import Language.Haskell.TH.Syntax

-- | Minimal complete definition: 'defaultDest' and 'defaultLoginRoute'.
class Yesod master => YesodAuth master where
    -- | Default destination on successful login or logout, if no other
    -- destination exists.
    defaultDest :: master -> Route master

    -- | Default page to redirect user to for logging in.
    defaultLoginRoute :: master -> Route master

    -- | Callback for a successful login.
    --
    -- The second parameter can contain various information, depending on login
    -- mechanism.
    onLogin :: Creds -> [(String, String)] -> GHandler (Auth master) master ()
    onLogin _ _ = return ()

    -- | Generate a random alphanumeric string.
    --
    -- This is used for verify string in email authentication.
    randomKey :: master -> IO String
    randomKey _ = do
        stdgen <- newStdGen
        return $ fst $ randomString 10 stdgen

-- | Each authentication subsystem (OpenId, Rpxnow, Email, Facebook) has its
-- own settings. If those settings are not present, then relevant handlers will
-- simply return a 404.
data Auth m = Auth
    { authIsOpenIdEnabled :: Bool
    , authRpxnowApiKey :: Maybe String
    , authEmailSettings :: Maybe (AuthEmailSettings m)
    -- | client id, secret and requested permissions
    , authFacebook :: Maybe (String, String, [String])
    }

-- | Which subsystem authenticated the user.
data AuthType = AuthOpenId | AuthRpxnow | AuthEmail | AuthFacebook
    deriving (Show, Read, Eq)

type Email = String
type VerKey = String
type VerUrl = String
type EmailId = Integer
type SaltedPass = String
type VerStatus = Bool

-- | Data stored in a database for each e-mail address.
data EmailCreds = EmailCreds
    { emailCredsId :: EmailId
    , emailCredsPass :: Maybe SaltedPass
    , emailCredsStatus :: VerStatus
    , emailCredsVerkey :: Maybe VerKey
    }

-- | For a sample set of settings for a trivial in-memory database, see
-- 'inMemoryEmailSettings'.
data AuthEmailSettings m = AuthEmailSettings
    { addUnverified :: Email -> VerKey -> GHandler (Auth m) m EmailId
    , sendVerifyEmail :: Email -> VerKey -> VerUrl -> GHandler (Auth m) m ()
    , getVerifyKey :: EmailId -> GHandler (Auth m) m (Maybe VerKey)
    , setVerifyKey :: EmailId -> VerKey -> GHandler (Auth m) m ()
    , verifyAccount :: EmailId -> GHandler (Auth m) m ()
    , setPassword :: EmailId -> String -> GHandler (Auth m) m ()
    , getEmailCreds :: Email -> GHandler (Auth m) m (Maybe EmailCreds)
    , getEmail :: EmailId -> GHandler (Auth m) m (Maybe Email)
    }

-- | User credentials
data Creds = Creds
    { credsIdent :: String -- ^ Identifier. Exact meaning depends on 'credsAuthType'.
    , credsAuthType :: AuthType -- ^ How the user was authenticated
    , credsEmail :: Maybe String -- ^ Verified e-mail address.
    , credsDisplayName :: Maybe String -- ^ Display name.
    , credsId :: Maybe Integer -- ^ Numeric ID, if used.
    , credsFacebookToken :: Maybe Facebook.AccessToken
    }
    deriving (Show, Read, Eq)

credsKey :: String
credsKey = "_CREDS"

setCreds :: YesodAuth master
         => Creds -> [(String, String)] -> GHandler (Auth master) master ()
setCreds creds extra = do
    setSession credsKey $ show creds
    onLogin creds extra

-- | Retrieves user credentials, if user is authenticated.
maybeCreds :: RequestReader r => r (Maybe Creds)
maybeCreds = do
    mstring <- lookupSession credsKey
    return $ mstring >>= readMay
  where
    readMay x = case reads x of
                    (y, _):_ -> Just y
                    _ -> Nothing

mkYesodSub "Auth master"
    [ ClassP ''YesodAuth [VarT $ mkName "master"]
    ]
    [$parseRoutes|
/check                   CheckR             GET
/logout                  LogoutR            GET
/openid                  OpenIdR            GET
/openid/forward          OpenIdForwardR     GET
/openid/complete         OpenIdCompleteR    GET
/login/rpxnow            RpxnowR

/facebook                FacebookR          GET
/facebook/start          StartFacebookR     GET

/register                EmailRegisterR     GET POST
/verify/#Integer/#String EmailVerifyR       GET
/login                   EmailLoginR        GET POST
/set-password            EmailPasswordR     GET POST
|]

testOpenId :: GHandler (Auth master) master ()
testOpenId = do
    a <- getYesodSub
    unless (authIsOpenIdEnabled a) notFound

getOpenIdR :: Yesod master => GHandler (Auth master) master RepHtml
getOpenIdR = do
    testOpenId
    lookupGetParam "dest" >>= maybe (return ()) setUltDestString
    rtom <- getRouteToMaster
    message <- getMessage
    defaultLayout $ do
        setTitle "Log in via OpenID"
        addBody [$hamlet|
$maybe message msg
    %p.message $msg$
%form!method=get!action=@rtom.OpenIdForwardR@
    %label!for=openid OpenID: $
    %input#openid!type=text!name=openid
    %input!type=submit!value=Login
|]

getOpenIdForwardR :: GHandler (Auth master) master ()
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
            redirect RedirectTemporary $ toMaster OpenIdR)
      (redirectString RedirectTemporary)
      res

getOpenIdCompleteR :: YesodAuth master => GHandler (Auth master) master ()
getOpenIdCompleteR = do
    testOpenId
    rr <- getRequest
    let gets' = reqGetParams rr
    res <- runAttemptT $ OpenId.authenticate gets'
    toMaster <- getRouteToMaster
    let onFailure err = do
        setMessage $ string $ show err
        redirect RedirectTemporary $ toMaster OpenIdR
    let onSuccess (OpenId.Identifier ident) = do
        y <- getYesod
        setCreds (Creds ident AuthOpenId Nothing Nothing Nothing Nothing) []
        redirectUltDest RedirectTemporary $ defaultDest y
    attempt onFailure onSuccess res

handleRpxnowR :: YesodAuth master => GHandler (Auth master) master ()
handleRpxnowR = do
    ay <- getYesod
    auth <- getYesodSub
    apiKey <- case authRpxnowApiKey auth of
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

getCheckR :: Yesod master => GHandler (Auth master) master RepHtmlJson
getCheckR = do
    creds <- maybeCreds
    defaultLayoutJson (do
        setTitle "Authentication Status"
        addBody $ html creds) (json creds)
  where
    html creds = [$hamlet|
%h1 Authentication Status
$if isNothing.creds
    %p Not logged in
$maybe creds c
    %p Logged in as $credsIdent.c$
|]
    json creds =
        jsonMap
            [ ("ident", jsonScalar $ maybe "" credsIdent creds)
            , ("displayName", jsonScalar $ fromMaybe ""
                                         $ creds >>= credsDisplayName)
            ]

getLogoutR :: YesodAuth master => GHandler (Auth master) master ()
getLogoutR = do
    y <- getYesod
    deleteSession credsKey
    redirectUltDest RedirectTemporary $ defaultDest y

-- | Retrieve user credentials. If user is not logged in, redirects to the
-- 'defaultLoginRoute'. Sets ultimate destination to current route, so user
-- should be sent back here after authenticating.
requireCreds :: YesodAuth master => GHandler sub master Creds
requireCreds =
    maybeCreds >>= maybe redirectLogin return
  where
    redirectLogin = do
        y <- getYesod
        setUltDest'
        redirect RedirectTemporary $ defaultLoginRoute y

getAuthEmailSettings :: GHandler (Auth master) master (AuthEmailSettings master)
getAuthEmailSettings = getYesodSub >>= maybe notFound return . authEmailSettings

getEmailRegisterR :: Yesod master => GHandler (Auth master) master RepHtml
getEmailRegisterR = do
    _ae <- getAuthEmailSettings
    toMaster <- getRouteToMaster
    defaultLayout $ setTitle "Register a new account" >> addBody [$hamlet|
%p Enter your e-mail address below, and a confirmation e-mail will be sent to you.
%form!method=post!action=@toMaster.EmailRegisterR@
    %label!for=email E-mail
    %input#email!type=email!name=email!width=150
    %input!type=submit!value=Register
|]

postEmailRegisterR :: YesodAuth master => GHandler (Auth master) master RepHtml
postEmailRegisterR = do
    ae <- getAuthEmailSettings
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
    let verUrl = render $ tm $ EmailVerifyR lid verKey
    sendVerifyEmail ae email verKey verUrl
    defaultLayout $ setTitle "Confirmation e-mail sent" >> addBody [$hamlet|
%p A confirmation e-mail has been sent to $email$.
|]

getEmailVerifyR :: YesodAuth master
           => Integer -> String -> GHandler (Auth master) master RepHtml
getEmailVerifyR lid key = do
    ae <- getAuthEmailSettings
    realKey <- getVerifyKey ae lid
    memail <- getEmail ae lid
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            verifyAccount ae lid
            setCreds (Creds email AuthEmail (Just email) Nothing (Just lid)
                      Nothing) []
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster EmailPasswordR
        _ -> defaultLayout $ do
            setTitle "Invalid verification key"
            addBody [$hamlet|
%p I'm sorry, but that was an invalid verification key.
|]

getEmailLoginR :: Yesod master => GHandler (Auth master) master RepHtml
getEmailLoginR = do
    _ae <- getAuthEmailSettings
    toMaster <- getRouteToMaster
    msg <- getMessage
    defaultLayout $ do
        setTitle "Login"
        addBody [$hamlet|
$maybe msg ms
    %p.message $ms$
%p Please log in to your account.
%p
    %a!href=@toMaster.EmailRegisterR@ I don't have an account
%form!method=post!action=@toMaster.EmailLoginR@
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
                %input!type=submit!value=Login
|]

postEmailLoginR :: YesodAuth master => GHandler (Auth master) master ()
postEmailLoginR = do
    ae <- getAuthEmailSettings
    (email, pass) <- runFormPost' $ (,)
        <$> emailInput "email"
        <*> stringInput "password"
    y <- getYesod
    mecreds <- getEmailCreds ae email
    let mlid =
            case mecreds of
                Just (EmailCreds lid (Just realpass) True _) ->
                    if isValidPass pass realpass then Just lid else Nothing
                _ -> Nothing
    case mlid of
        Just lid -> do
            setCreds (Creds email AuthEmail (Just email) Nothing (Just lid)
                      Nothing) []
            redirectUltDest RedirectTemporary $ defaultDest y
        Nothing -> do
            setMessage $ string "Invalid email/password combination"
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster EmailLoginR

getEmailPasswordR :: Yesod master => GHandler (Auth master) master RepHtml
getEmailPasswordR = do
    _ae <- getAuthEmailSettings
    toMaster <- getRouteToMaster
    mcreds <- maybeCreds
    case mcreds of
        Just (Creds _ AuthEmail _ _ (Just _) _) -> return ()
        _ -> do
            setMessage $ string "You must be logged in to set a password"
            redirect RedirectTemporary $ toMaster EmailLoginR
    msg <- getMessage
    defaultLayout $ do
        setTitle "Set password"
        addBody [$hamlet|
$maybe msg ms
    %p.message $ms$
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

postEmailPasswordR :: YesodAuth master => GHandler (Auth master) master ()
postEmailPasswordR = do
    ae <- getAuthEmailSettings
    (new, confirm) <- runFormPost' $ (,)
        <$> stringInput "new"
        <*> stringInput "confirm"
    toMaster <- getRouteToMaster
    when (new /= confirm) $ do
        setMessage $ string "Passwords did not match, please try again"
        redirect RedirectTemporary $ toMaster EmailPasswordR
    mcreds <- maybeCreds
    lid <- case mcreds of
            Just (Creds _ AuthEmail _ _ (Just lid) _) -> return lid
            _ -> do
                setMessage $ string "You must be logged in to set a password"
                redirect RedirectTemporary $ toMaster EmailLoginR
    salted <- liftIO $ saltPass new
    setPassword ae lid salted
    setMessage $ string "Password updated"
    redirect RedirectTemporary $ toMaster EmailLoginR

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

-- | A simplistic set of email settings, useful only for testing purposes. In
-- particular, it doesn't actually send emails, but instead prints verification
-- URLs to stderr.
inMemoryEmailSettings :: IO (AuthEmailSettings a)
inMemoryEmailSettings = do
    mm <- newMVar []
    return AuthEmailSettings
        { addUnverified = \email verkey -> liftIO $ modifyMVar mm $ \m -> do
            let helper (_, EmailCreds x _ _ _) = x
            let newId = 1 + maximum (0 : map helper m)
            let ec = EmailCreds newId Nothing False $ Just verkey
            return ((email, ec) : m, newId)
        , sendVerifyEmail = \_email _verkey verurl -> liftIO $
            hPutStrLn stderr $ "Please go to: " ++ verurl
        , getVerifyKey = \eid -> liftIO $ withMVar mm $ \m -> return $
            join $ lookup eid $ map (\(_, EmailCreds eid' _ _ vk) -> (eid', vk)) m
        , setVerifyKey = \eid key -> liftIO $ modifyMVar_ mm $ \m -> return $
            map (setHelper eid key) m
        , verifyAccount = \eid -> liftIO $ modifyMVar_ mm $ return . map (vago eid)
        , setPassword = \eid pass -> liftIO $ modifyMVar_ mm $ return . map (spgo eid pass)
        , getEmailCreds = \email -> liftIO $ withMVar mm $ return . lookup email
        , getEmail = \eid -> liftIO $ withMVar mm $ \m -> return $
            case filter (\(_, EmailCreds eid' _ _ _) -> eid == eid') m of
                ((email, _):_) -> Just email
                _ -> Nothing
        }
  where
    setHelper eid key pair@(k, EmailCreds eid' b c _)
        | eid == eid' = (k, EmailCreds eid b c $ Just key)
        | otherwise = pair
    vago eid (email, EmailCreds eid' pass status key)
        | eid == eid' = (email, EmailCreds eid pass True key)
        | otherwise = (email, EmailCreds eid' pass status key)
    spgo eid pass (email, EmailCreds eid' pass' status key)
        | eid == eid' = (email, EmailCreds eid (Just pass) status key)
        | otherwise = (email, EmailCreds eid' pass' status key)

getFacebookR :: YesodAuth master => GHandler (Auth master) master ()
getFacebookR = do
    y <- getYesod
    a <- authFacebook <$> getYesodSub
    case a of
        Nothing -> notFound
        Just (cid, secret, _) -> do
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

getStartFacebookR :: GHandler (Auth master) master ()
getStartFacebookR = do
    y <- getYesodSub
    case authFacebook y of
        Nothing -> notFound
        Just (cid, secret, perms) -> do
            render <- getUrlRender
            tm <- getRouteToMaster
            let fb = Facebook.Facebook cid secret $ render $ tm FacebookR
            let fburl = Facebook.getForwardUrl fb perms
            redirectString RedirectTemporary fburl
