{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
module Yesod.Auth.Email
    ( -- * Plugin
      authEmail
    , YesodAuthEmail (..)
    , EmailCreds (..)
    , saltPass
      -- * Routes
    , loginR
    , registerR
    , forgotPasswordR
    , setpassR
    , isValidPass
      -- * Types
    , Email
    , VerKey
    , VerUrl
    , SaltedPass
    , VerStatus
    , Identifier
     -- * Misc
    , loginLinkKey
    , setLoginLinkKey
    ) where

import Network.Mail.Mime (randomString)
import Yesod.Auth
import System.Random
import Data.Digest.Pure.MD5
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text (Text)
import Yesod.Core
import qualified Crypto.PasswordStore as PS
import qualified Text.Email.Validate
import qualified Yesod.Auth.Message as Msg
import Control.Applicative ((<$>), (<*>))
import Yesod.Form
import Control.Monad (when)
import Data.Time (getCurrentTime, addUTCTime)
import Safe (readMay)

loginR, registerR, forgotPasswordR, setpassR :: AuthRoute
loginR = PluginR "email" ["login"]
registerR = PluginR "email" ["register"]
forgotPasswordR = PluginR "email" ["forgot-password"]
setpassR = PluginR "email" ["set-password"]

verify :: Text -> Text -> AuthRoute -- FIXME
verify eid verkey = PluginR "email" ["verify", eid, verkey]

type Email = Text
type VerKey = Text
type VerUrl = Text
type SaltedPass = Text
type VerStatus = Bool

-- | An Identifier generalizes an email address to allow users to log in with
-- some other form of credentials (e.g., username).
--
-- Note that any of these other identifiers must not be valid email addresses.
--
-- Since 1.2.0
type Identifier = Text

-- | Data stored in a database for each e-mail address.
data EmailCreds site = EmailCreds
    { emailCredsId :: AuthEmailId site
    , emailCredsAuthId :: Maybe (AuthId site)
    , emailCredsStatus :: VerStatus
    , emailCredsVerkey :: Maybe VerKey
    , emailCredsEmail :: Email
    }

class (YesodAuth site, PathPiece (AuthEmailId site)) => YesodAuthEmail site where
    type AuthEmailId site

    -- | Add a new email address to the database, but indicate that the address
    -- has not yet been verified.
    --
    -- Since 1.1.0
    addUnverified :: Email -> VerKey -> HandlerT site IO (AuthEmailId site)

    -- | Send an email to the given address to verify ownership.
    --
    -- Since 1.1.0
    sendVerifyEmail :: Email -> VerKey -> VerUrl -> HandlerT site IO ()

    -- | Get the verification key for the given email ID.
    --
    -- Since 1.1.0
    getVerifyKey :: AuthEmailId site -> HandlerT site IO (Maybe VerKey)

    -- | Set the verification key for the given email ID.
    --
    -- Since 1.1.0
    setVerifyKey :: AuthEmailId site -> VerKey -> HandlerT site IO ()

    -- | Verify the email address on the given account.
    --
    -- Since 1.1.0
    verifyAccount :: AuthEmailId site -> HandlerT site IO (Maybe (AuthId site))

    -- | Get the salted password for the given account.
    --
    -- Since 1.1.0
    getPassword :: AuthId site -> HandlerT site IO (Maybe SaltedPass)

    -- | Set the salted password for the given account.
    --
    -- Since 1.1.0
    setPassword :: AuthId site -> SaltedPass -> HandlerT site IO ()

    -- | Get the credentials for the given @Identifier@, which may be either an
    -- email address or some other identification (e.g., username).
    --
    -- Since 1.2.0
    getEmailCreds :: Identifier -> HandlerT site IO (Maybe (EmailCreds site))

    -- | Get the email address for the given email ID.
    --
    -- Since 1.1.0
    getEmail :: AuthEmailId site -> HandlerT site IO (Maybe Email)

    -- | Generate a random alphanumeric string.
    --
    -- Since 1.1.0
    randomKey :: site -> IO Text
    randomKey _ = do
        stdgen <- newStdGen
        return $ TS.pack $ fst $ randomString 10 stdgen

    -- | Route to send user to after password has been set correctly.
    --
    -- Since 1.2.0
    afterPasswordRoute :: site -> Route site

    -- | Does the user need to provide the current password in order to set a
    -- new password?
    --
    -- Default: if the user logged in via an email link do not require a password.
    --
    -- Since 1.2.1
    needOldPassword :: AuthId site -> HandlerT site IO Bool
    needOldPassword aid' = do
        mkey <- lookupSession loginLinkKey
        case mkey >>= readMay . TS.unpack of
            Just (aidT, time) | Just aid <- fromPathPiece aidT, toPathPiece (aid `asTypeOf` aid') == toPathPiece aid' -> do
                now <- liftIO getCurrentTime
                return $ addUTCTime (60 * 30) time <= now
            _ -> return True

    -- | Check that the given plain-text password meets minimum security standards.
    --
    -- Default: password is at least three characters.
    checkPasswordSecurity :: AuthId site -> Text -> HandlerT site IO (Either Text ())
    checkPasswordSecurity _ x
        | TS.length x >= 3 = return $ Right ()
        | otherwise = return $ Left "Password must be at least three characters"

    -- | Response after sending a confirmation email.
    --
    -- Since 1.2.2
    confirmationEmailSentResponse :: Text -> HandlerT site IO Html
    confirmationEmailSentResponse identifier = authLayout $ do
        setTitleI Msg.ConfirmationEmailSentTitle
        [whamlet|<p>_{Msg.ConfirmationEmailSent identifier}|]

    -- | Additional normalization of email addresses, besides standard canonicalization.
    --
    -- Default: do nothing. Note that in future versions of Yesod, the default
    -- will change to lower casing the email address. At that point, you will
    -- need to either ensure your database values are migrated to lower case,
    -- or change this default back to doing nothing.
    --
    -- Since 1.2.3
    normalizeEmailAddress :: site -> Text -> Text
    normalizeEmailAddress _ = TS.toLower

authEmail :: YesodAuthEmail m => AuthPlugin m
authEmail =
    AuthPlugin "email" dispatch $ \tm ->
        [whamlet|
$newline never
<form method="post" action="@{tm loginR}">
    <table>
        <tr>
            <th>_{Msg.Email}
            <td>
                <input type="email" name="email">
        <tr>
            <th>_{Msg.Password}
            <td>
                <input type="password" name="password">
        <tr>
            <td colspan="2">
                <input type="submit" value=_{Msg.LoginViaEmail}>
                <a href="@{tm registerR}">I don't have an account
|]
  where
    dispatch "GET" ["register"] = getRegisterR >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR >>= sendResponse
    dispatch "GET" ["forgot-password"] = getForgotPasswordR >>= sendResponse
    dispatch "POST" ["forgot-password"] = postForgotPasswordR >>= sendResponse
    dispatch "GET" ["verify", eid, verkey] =
        case fromPathPiece eid of
            Nothing -> notFound
            Just eid' -> getVerifyR eid' verkey >>= sendResponse
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["set-password"] = getPasswordR >>= sendResponse
    dispatch "POST" ["set-password"] = postPasswordR >>= sendResponse
    dispatch _ _ = notFound

getRegisterR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
getRegisterR = do
    email <- newIdent
    tp <- getRouteToParent
    lift $ authLayout $ do
        setTitleI Msg.RegisterLong
        [whamlet|
            <p>_{Msg.EnterEmail}
            <form method="post" action="@{tp registerR}">
                <div id="registerForm">
                    <label for=#{email}>_{Msg.Email}:
                    <input ##{email} type="email" name="email" width="150" autofocus>
                <button .btn>_{Msg.Register}
        |]

registerHelper :: YesodAuthEmail master
               => Bool -- ^ allow usernames?
               -> Route Auth
               -> HandlerT Auth (HandlerT master IO) Html
registerHelper allowUsername dest = do
    y <- lift getYesod
    midentifier <- lookupPostParam "email"
    identifier <-
        case midentifier of
            Nothing -> loginErrorMessageI dest Msg.NoIdentifierProvided
            Just x
                | Just x' <- Text.Email.Validate.canonicalizeEmail (encodeUtf8 x) ->
                    return $ normalizeEmailAddress y $ decodeUtf8With lenientDecode x'
                | allowUsername -> return $ TS.strip x
                | otherwise -> loginErrorMessageI dest Msg.InvalidEmailAddress

    mecreds <- lift $ getEmailCreds identifier
    (lid, verKey, email) <-
        case mecreds of
            Just (EmailCreds lid _ _ (Just key) email) -> return (lid, key, email)
            Just (EmailCreds lid _ _ Nothing email) -> do
                key <- liftIO $ randomKey y
                lift $ setVerifyKey lid key
                return (lid, key, email)
            Nothing
                | allowUsername ->
                    loginErrorMessageI dest (Msg.IdentifierNotFound identifier)
                | otherwise -> do
                    key <- liftIO $ randomKey y
                    lid <- lift $ addUnverified identifier key
                    return (lid, key, identifier)
    render <- getUrlRender
    let verUrl = render $ verify (toPathPiece lid) verKey
    lift $ sendVerifyEmail email verKey verUrl
    lift $ confirmationEmailSentResponse identifier

postRegisterR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
postRegisterR = registerHelper False registerR

getForgotPasswordR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
getForgotPasswordR = do
    tp <- getRouteToParent
    email <- newIdent
    lift $ authLayout $ do
        setTitleI Msg.PasswordResetTitle
        [whamlet|
            <p>_{Msg.PasswordResetPrompt}
            <form method="post" action="@{tp forgotPasswordR}">
                <div id="registerForm">
                    <label for=#{email}>_{Msg.ProvideIdentifier}
                    <input ##{email} type=text name="email" width="150" autofocus>
                <button .btn>_{Msg.SendPasswordResetEmail}
        |]

postForgotPasswordR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
postForgotPasswordR = registerHelper True forgotPasswordR

getVerifyR :: YesodAuthEmail site
           => AuthEmailId site
           -> Text
           -> HandlerT Auth (HandlerT site IO) Html
getVerifyR lid key = do
    realKey <- lift $ getVerifyKey lid
    memail <- lift $ getEmail lid
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            muid <- lift $ verifyAccount lid
            case muid of
                Nothing -> return ()
                Just uid -> do
                    lift $ setCreds False $ Creds "email-verify" email [("verifiedEmail", email)] -- FIXME uid?
                    lift $ setMessageI Msg.AddressVerified
                    lift $ setLoginLinkKey uid
                    redirect setpassR
        _ -> return ()
    lift $ authLayout $ do
        setTitleI Msg.InvalidKey
        [whamlet|
$newline never
<p>_{Msg.InvalidKey}
|]

postLoginR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) ()
postLoginR = do
    (identifier, pass) <- lift $ runInputPost $ (,)
        <$> ireq textField "email"
        <*> ireq textField "password"
    mecreds <- lift $ getEmailCreds identifier
    maid <-
        case ( mecreds >>= emailCredsAuthId
             , emailCredsEmail <$> mecreds
             , emailCredsStatus <$> mecreds
             ) of
            (Just aid, Just email, Just True) -> do
                mrealpass <- lift $ getPassword aid
                case mrealpass of
                    Nothing -> return Nothing
                    Just realpass -> return $
                        if isValidPass pass realpass
                            then Just email
                            else Nothing
            _ -> return Nothing
    let isEmail = Text.Email.Validate.isValid $ encodeUtf8 identifier
    case maid of
        Just email ->
            lift $ setCreds True $ Creds
                (if isEmail then "email" else "username")
                email
                [("verifiedEmail", email)]
        Nothing -> do
            loginErrorMessageI LoginR $
                if isEmail
                    then Msg.InvalidEmailPass
                    else Msg.InvalidUsernamePass

getPasswordR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
getPasswordR = do
    maid <- lift maybeAuthId
    pass0 <- newIdent
    pass1 <- newIdent
    pass2 <- newIdent
    case maid of
        Just _ -> return ()
        Nothing -> loginErrorMessageI LoginR Msg.BadSetPass
    tp <- getRouteToParent
    needOld <- maybe (return True) (lift . needOldPassword) maid
    lift $ authLayout $ do
        setTitleI Msg.SetPassTitle
        [whamlet|
$newline never
<h3>_{Msg.SetPass}
<form method="post" action="@{tp setpassR}">
    <table>
        $if needOld
            <tr>
                <th>
                    <label for=#{pass0}>Current Password
                <td>
                    <input ##{pass0} type="password" name="current" autofocus>
        <tr>
            <th>
                <label for=#{pass1}>_{Msg.NewPass}
            <td>
                <input ##{pass1} type="password" name="new" :not needOld:autofocus>
        <tr>
            <th>
                <label for=#{pass2}>_{Msg.ConfirmPass}
            <td>
                <input ##{pass2} type="password" name="confirm">
        <tr>
            <td colspan="2">
                <input type="submit" value=_{Msg.SetPassTitle}>
|]

postPasswordR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) ()
postPasswordR = do
    maid <- lift maybeAuthId
    aid <- case maid of
            Nothing -> loginErrorMessageI LoginR Msg.BadSetPass
            Just aid -> return aid

    tm <- getRouteToParent
    
    needOld <- lift $ needOldPassword aid
    when needOld $ do
        current <- lift $ runInputPost $ ireq textField "current"
        mrealpass <- lift $ getPassword aid
        case mrealpass of
            Nothing ->
                lift $ loginErrorMessage (tm setpassR) "You do not currently have a password set on your account"
            Just realpass
                | isValidPass current realpass -> return ()
                | otherwise ->
                    lift $ loginErrorMessage (tm setpassR) "Invalid current password, please try again"

    (new, confirm) <- lift $ runInputPost $ (,)
        <$> ireq textField "new"
        <*> ireq textField "confirm"
    when (new /= confirm) $
        loginErrorMessageI setpassR Msg.PassMismatch

    isSecure <- lift $ checkPasswordSecurity aid new
    case isSecure of
        Left e -> lift $ loginErrorMessage (tm setpassR) e
        Right () -> return ()

    salted <- liftIO $ saltPass new
    lift $ do
        y <- getYesod
        setPassword aid salted
        setMessageI Msg.PassUpdated
        deleteSession loginLinkKey
        redirect $ afterPasswordRoute y

saltLength :: Int
saltLength = 5

-- | Salt a password with a randomly generated salt.
saltPass :: Text -> IO Text
saltPass = fmap (decodeUtf8With lenientDecode)
         . flip PS.makePassword 12
         . encodeUtf8

saltPass' :: String -> String -> String
saltPass' salt pass = salt ++ show (md5 $ TLE.encodeUtf8 $ TL.pack $ salt ++ pass)

isValidPass :: Text -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass ct salted =
    PS.verifyPassword (encodeUtf8 ct) (encodeUtf8 salted) || isValidPass' ct salted

isValidPass' :: Text -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass' clear' salted' =
    let salt = take saltLength salted
     in salted == saltPass' salt clear
  where
    clear = TS.unpack clear'
    salted = TS.unpack salted'

-- | Session variable set when user logged in via a login link. See
-- 'needOldPassword'.
--
-- Since 1.2.1
loginLinkKey :: Text
loginLinkKey = "_AUTH_EMAIL_LOGIN_LINK"

-- | Set 'loginLinkKey' to the current time.
--
-- Since 1.2.1
setLoginLinkKey :: (YesodAuthEmail site, MonadHandler m, HandlerSite m ~ site) => AuthId site -> m ()
setLoginLinkKey aid = do
    now <- liftIO getCurrentTime
    setSession loginLinkKey $ TS.pack $ show (toPathPiece aid, now)
