{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
module Yesod.Helpers.EmailAuth
    ( getEmailAuth
    , EmailAuth
    , siteEmailAuth
    , EmailAuthRoutes (..)
    , YesodEmailAuth (..)
    ) where

import Yesod
import Yesod.Helpers.Auth
import System.Random
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Digest.Pure.MD5

class Yesod y => YesodEmailAuth y where
    addUnverified :: y
                  -> String -- ^ email
                  -> String -- ^ verification key
                  -> IO Integer -- ^ login_id
    sendVerifyEmail :: y
                    -> String -- ^ email
                    -> String -- ^ verification key
                    -> String -- ^ verify URL
                    -> IO ()
    getVerifyKey :: y
                 -> Integer -- ^ login_id
                 -> IO (Maybe String)
    verifyAccount :: y
                  -> Integer -- ^ login_id
                  -> IO ()
    setPassword :: y
                -> Integer -- ^ login_id
                -> String -- ^ salted password
                -> IO ()
    getCreds :: y
             -> String -- ^ email address
             -> IO (Maybe (Integer, Maybe String, Bool, String)) -- ^ id, salted pass, is verified, verify key
    getEmail :: y -> Integer -> IO (Maybe String)

    randomKey :: y -> IO String
    randomKey _ = do
        stdgen <- newStdGen
        return $ take 10 $ randomRs ('A', 'Z') stdgen

    onSuccessfulLogin :: y -> Routes y
    onSuccessfulLogout :: y -> Routes y

    onEmailAuthLogin :: y
                     -> String -- ^ email
                     -> Integer -- ^ login_id
                     -> IO ()

data EmailAuth = EmailAuth

getEmailAuth :: a -> EmailAuth
getEmailAuth _ = EmailAuth

mkYesodSub "EmailAuth" [''YesodEmailAuth] [$parseRoutes|
/register       RegisterR      GET POST
/verify/#/$     VerifyR        GET
/login          LoginR         GET POST
/set-password   PasswordR      GET POST
/logout         LogoutR        GET
|]

getRegisterR :: Yesod master => GHandler EmailAuth master RepHtml
getRegisterR = do
    toMaster <- getRouteToMaster
    applyLayout "Register a new account" (return ()) [$hamlet|
%p Enter your e-mail address below, and a confirmation e-mail will be sent to you.
%form!method=post!action=@toMaster.RegisterR@
    %label!for=email E-mail
    %input#email!type=email!name=email!width=150
    %input!type=submit!value=Register
|]

postRegisterR :: YesodEmailAuth master => GHandler EmailAuth master RepHtml
postRegisterR = do
    email <- runFormPost $ checkEmail $ required $ input "email"
    y <- getYesod
    creds <- liftIO $ getCreds y email
    (lid, verKey) <-
        case creds of
            Nothing -> liftIO $ do
                key <- randomKey y
                lid <- addUnverified y email key
                return (lid, key)
            Just (lid, _, _, key) -> return (lid, key)
    render <- getUrlRender
    tm <- getRouteToMaster
    let verUrl = render $ tm $ VerifyR lid verKey
    liftIO $ sendVerifyEmail y email verKey verUrl
    applyLayout "Confirmation e-mail sent" (return ()) [$hamlet|
%p A confirmation e-mail has been sent to $cs.email$.
|]

checkEmail :: Form ParamValue -> Form ParamValue
checkEmail = notEmpty -- FIXME

getVerifyR :: YesodEmailAuth master
           => Integer -> String -> GHandler EmailAuth master RepHtml
getVerifyR lid key = do
    y <- getYesod
    realKey <- liftIO $ getVerifyKey y lid
    memail <- liftIO $ getEmail y lid
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            liftIO $ verifyAccount y lid
            setLoginSession email lid
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster PasswordR
        _ -> applyLayout "Invalid verification key" (return ()) [$hamlet|
%p I'm sorry, but that was an invalid verification key.
        |]

messageKey :: String
messageKey = "MESSAGE"

getMessage :: GHandler sub master (Maybe HtmlContent)
getMessage = do
    s <- session
    clearSession messageKey
    return $ listToMaybe $ map (Encoded . cs) $ s messageKey

setMessage :: String -> GHandler sub master ()
setMessage = setSession messageKey . cs

getLoginR :: Yesod master => GHandler EmailAuth master RepHtml
getLoginR = do
    toMaster <- getRouteToMaster
    msg <- getMessage
    applyLayout "Login" (return ()) [$hamlet|
$maybe msg ms
    %p.message $ms$
%p Please log in to your account.
%p
    %a!href=@toMaster.RegisterR@ I don't have an account
%form!method=post!action=@toMaster.LoginR@
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

postLoginR :: YesodEmailAuth master => GHandler EmailAuth master ()
postLoginR = do
    (email, pass) <- runFormPost $ (,)
        <$> checkEmail (required $ input "email")
        <*> required (input "password")
    y <- getYesod
    creds <- liftIO $ getCreds y email
    let mlid =
            case creds of
                Just (lid, Just realpass, True, _) ->
                    if isValidPass pass realpass then Just lid else Nothing
                _ -> Nothing
    case mlid of
        Just lid -> do
            setLoginSession email lid
            redirect RedirectTemporary $ onSuccessfulLogin y
        Nothing -> do
            setMessage "Invalid email/password combination"
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

getPasswordR :: Yesod master => GHandler EmailAuth master RepHtml
getPasswordR = do
    l <- isJust <$> isLoggedIn
    toMaster <- getRouteToMaster
    unless l $ do
        setMessage "You must be logged in to set a password"
        redirect RedirectTemporary $ toMaster LoginR
    msg <- getMessage
    applyLayout "Set password" (return ()) [$hamlet|
$maybe msg ms
    %p.message $ms$
%h3 Set a new password
%form!method=post!action=@toMaster.PasswordR@
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

postPasswordR :: YesodEmailAuth master => GHandler EmailAuth master ()
postPasswordR = do
    (new, confirm) <- runFormPost $ (,)
        <$> notEmpty (required $ input "new")
        <*> notEmpty (required $ input "confirm")
    toMaster <- getRouteToMaster
    when (new /= confirm) $ do
        setMessage "Passwords did not match, please try again"
        redirect RedirectTemporary $ toMaster PasswordR
    mlid <- isLoggedIn
    lid <- case mlid of
            Just lid -> return lid
            Nothing -> do
                setMessage "You must be logged in to set a password"
                redirect RedirectTemporary $ toMaster LoginR
    salted <- liftIO $ saltPass new
    y <- getYesod
    liftIO $ setPassword y lid salted
    setMessage "Password updated"
    redirect RedirectTemporary $ toMaster LoginR

getLogoutR :: YesodEmailAuth master => GHandler EmailAuth master RepHtml
getLogoutR = do
    clearSession identKey
    clearSession displayNameKey
    clearSession emailAuthIdKey
    y <- getYesod
    redirect RedirectTemporary $ onSuccessfulLogout y

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

emailAuthIdKey :: String
emailAuthIdKey = "EMAIL_AUTH_ID"

setLoginSession :: YesodEmailAuth master
                => String -> Integer -> GHandler sub master ()
setLoginSession email lid = do
    setSession identKey email
    setSession displayNameKey email
    setSession emailAuthIdKey $ show lid
    y <- getYesod
    liftIO $ onEmailAuthLogin y email lid

isLoggedIn :: GHandler sub master (Maybe Integer)
isLoggedIn = do
    s <- session
    return $
        if null (s identKey)
            then Nothing
            else listToMaybe (s emailAuthIdKey) >>= readMay

readMay :: String -> Maybe Integer
readMay s = case reads s of
                [] -> Nothing
                ((i, _):_) -> Just i
