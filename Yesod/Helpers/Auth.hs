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
    , AuthEmailSettings (..)
    , inMemoryEmailSettings
    ) where

import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Yesod

import Control.Monad.Attempt
import Data.Maybe
import Control.Monad
import System.Random
import Data.Digest.Pure.MD5
import Control.Applicative
import Control.Concurrent.MVar
import System.IO

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

    -- | Generate a random alphanumeric string.
    --
    -- This is used for verify string in email authentication.
    randomKey :: master -> IO String
    randomKey _ = do
        stdgen <- newStdGen
        return $ take 10 $ randomRs ('A', 'Z') stdgen

data Auth = Auth
    { authIsOpenIdEnabled :: Bool
    , authRpxnowApiKey :: Maybe String
    , authEmailSettings :: Maybe AuthEmailSettings
    }

data AuthType = AuthOpenId | AuthRpxnow | AuthEmail
    deriving (Show, Read, Eq)

type Email = String
type VerKey = String
type VerUrl = String
type EmailId = Integer
type SaltedPass = String
type VerStatus = Bool
data EmailCreds = EmailCreds EmailId (Maybe SaltedPass) VerStatus VerKey
data AuthEmailSettings = AuthEmailSettings
    { addUnverified :: Email -> VerKey -> IO EmailId
    , sendVerifyEmail :: Email -> VerKey -> VerUrl -> IO ()
    , getVerifyKey :: EmailId -> IO (Maybe VerKey)
    , verifyAccount :: EmailId -> IO ()
    , setPassword :: EmailId -> String -> IO ()
    , getEmailCreds :: Email -> IO (Maybe EmailCreds)
    , getEmail :: EmailId -> IO (Maybe Email)
    }

-- | User credentials
data Creds = Creds
    { credsIdent :: String -- ^ Identifier. Exact meaning depends on 'credsAuthType'.
    , credsAuthType :: AuthType -- ^ How the user was authenticated
    , credsEmail :: Maybe String -- ^ Verified e-mail address.
    , credsDisplayName :: Maybe String -- ^ Display name.
    , credsId :: Maybe Integer -- ^ Numeric ID, if used.
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

/register              EmailRegisterR     GET POST
/verify/#/$            EmailVerifyR       GET
/login                 EmailLoginR        GET POST
/set-password          EmailPasswordR     GET POST
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
        setCreds (Creds ident AuthOpenId Nothing Nothing Nothing) []
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
                    Nothing
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

getAuthEmailSettings :: GHandler Auth master AuthEmailSettings
getAuthEmailSettings = getYesodSub >>= maybe notFound return . authEmailSettings

getEmailRegisterR :: Yesod master => GHandler Auth master RepHtml
getEmailRegisterR = do
    _ae <- getAuthEmailSettings
    toMaster <- getRouteToMaster
    applyLayout "Register a new account" (return ()) [$hamlet|
%p Enter your e-mail address below, and a confirmation e-mail will be sent to you.
%form!method=post!action=@toMaster.EmailRegisterR@
    %label!for=email E-mail
    %input#email!type=email!name=email!width=150
    %input!type=submit!value=Register
|]

postEmailRegisterR :: YesodAuth master => GHandler Auth master RepHtml
postEmailRegisterR = do
    ae <- getAuthEmailSettings
    email <- runFormPost $ checkEmail $ required $ input "email"
    y <- getYesod
    mecreds <- liftIO $ getEmailCreds ae email
    (lid, verKey) <-
        case mecreds of
            Nothing -> liftIO $ do
                key <- randomKey y
                lid <- addUnverified ae email key
                return (lid, key)
            Just (EmailCreds lid _ _ key) -> return (lid, key)
    render <- getUrlRender
    tm <- getRouteToMaster
    let verUrl = render $ tm $ EmailVerifyR lid verKey
    liftIO $ sendVerifyEmail ae email verKey verUrl
    applyLayout "Confirmation e-mail sent" (return ()) [$hamlet|
%p A confirmation e-mail has been sent to $cs.email$.
|]

checkEmail :: Form ParamValue -> Form ParamValue
checkEmail = notEmpty -- FIXME

getEmailVerifyR :: YesodAuth master
           => Integer -> String -> GHandler Auth master RepHtml
getEmailVerifyR lid key = do
    ae <- getAuthEmailSettings
    realKey <- liftIO $ getVerifyKey ae lid
    memail <- liftIO $ getEmail ae lid
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            liftIO $ verifyAccount ae lid
            setCreds (Creds email AuthEmail (Just email) Nothing (Just lid)) []
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster EmailPasswordR
        _ -> applyLayout "Invalid verification key" (return ()) [$hamlet|
%p I'm sorry, but that was an invalid verification key.
        |]

getEmailLoginR :: Yesod master => GHandler Auth master RepHtml
getEmailLoginR = do
    _ae <- getAuthEmailSettings
    toMaster <- getRouteToMaster
    msg <- getMessage
    applyLayout "Login" (return ()) [$hamlet|
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

postEmailLoginR :: YesodAuth master => GHandler Auth master ()
postEmailLoginR = do
    ae <- getAuthEmailSettings
    (email, pass) <- runFormPost $ (,)
        <$> checkEmail (required $ input "email")
        <*> required (input "password")
    y <- getYesod
    mecreds <- liftIO $ getEmailCreds ae email
    let mlid =
            case mecreds of
                Just (EmailCreds lid (Just realpass) True _) ->
                    if isValidPass pass realpass then Just lid else Nothing
                _ -> Nothing
    case mlid of
        Just lid -> do
            setCreds (Creds email AuthEmail (Just email) Nothing (Just lid)) []
            redirectUltDest RedirectTemporary $ defaultDest y
        Nothing -> do
            setMessage $ cs "Invalid email/password combination"
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster EmailLoginR

getEmailPasswordR :: Yesod master => GHandler Auth master RepHtml
getEmailPasswordR = do
    _ae <- getAuthEmailSettings
    toMaster <- getRouteToMaster
    mcreds <- maybeCreds
    case mcreds of
        Just (Creds _ AuthEmail _ _ (Just _)) -> return ()
        _ -> do
            setMessage $ cs "You must be logged in to set a password"
            redirect RedirectTemporary $ toMaster EmailLoginR
    msg <- getMessage
    applyLayout "Set password" (return ()) [$hamlet|
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

postEmailPasswordR :: YesodAuth master => GHandler Auth master ()
postEmailPasswordR = do
    ae <- getAuthEmailSettings
    (new, confirm) <- runFormPost $ (,)
        <$> notEmpty (required $ input "new")
        <*> notEmpty (required $ input "confirm")
    toMaster <- getRouteToMaster
    when (new /= confirm) $ do
        setMessage $ cs "Passwords did not match, please try again"
        redirect RedirectTemporary $ toMaster EmailPasswordR
    mcreds <- maybeCreds
    lid <- case mcreds of
            Just (Creds _ AuthEmail _ _ (Just lid)) -> return lid
            _ -> do
                setMessage $ cs "You must be logged in to set a password"
                redirect RedirectTemporary $ toMaster EmailLoginR
    salted <- liftIO $ saltPass new
    liftIO $ setPassword ae lid salted
    setMessage $ cs "Password updated"
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

saltPass' :: String -> String -> String -- FIXME better salting scheme?
saltPass' salt pass = salt ++ show (md5 $ cs $ salt ++ pass)

inMemoryEmailSettings :: IO AuthEmailSettings
inMemoryEmailSettings = do
    mm <- newMVar []
    return $ AuthEmailSettings
        { addUnverified = \email verkey -> modifyMVar mm $ \m -> do
            let helper (_, EmailCreds x _ _ _) = x
            let newId = 1 + maximum (0 : map helper m)
            let ec = EmailCreds newId Nothing False verkey
            return ((email, ec) : m, newId)
        , sendVerifyEmail = \_email _verkey verurl ->
            hPutStrLn stderr $ "Please go to: " ++ verurl
        , getVerifyKey = \eid -> withMVar mm $ \m -> return $
            lookup eid $ map (\(_, EmailCreds eid' _ _ vk) -> (eid', vk)) m
        , verifyAccount = \eid -> modifyMVar_ mm $ return . map (vago eid)
        , setPassword = \eid pass -> modifyMVar_ mm $ return . map (spgo eid pass)
        , getEmailCreds = \email -> withMVar mm $ return . lookup email
        , getEmail = \eid -> withMVar mm $ \m -> return $
            case filter (\(_, EmailCreds eid' _ _ _) -> eid == eid') m of
                ((email, _):_) -> Just email
                _ -> Nothing
        }
  where
    vago eid (email, EmailCreds eid' pass status key)
        | eid == eid' = (email, EmailCreds eid pass True key)
        | otherwise = (email, EmailCreds eid' pass status key)
    spgo eid pass (email, EmailCreds eid' pass' status key)
        | eid == eid' = (email, EmailCreds eid (Just pass) status key)
        | otherwise = (email, EmailCreds eid' pass' status key)
