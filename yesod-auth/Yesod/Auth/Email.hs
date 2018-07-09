{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE PatternGuards           #-}
{-# LANGUAGE QuasiQuotes             #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
-- | A Yesod plugin for Authentication via e-mail
--
-- This plugin works out of the box by only setting a few methods on
-- the type class that tell the plugin how to interoperate with your
-- user data storage (your database).  However, almost everything is
-- customizeable by setting more methods on the type class.  In
-- addition, you can send all the form submissions via JSON and
-- completely control the user's flow.
--
-- This is a standard registration e-mail flow
--
-- 1. A user registers a new e-mail address, and an e-mail is sent there
-- 2. The user clicks on the registration link in the e-mail. Note that
--   at this point they are actually logged in (without a
--   password). That means that when they log out they will need to
--  reset their password.
-- 3. The user sets their password and is redirected to the site.
-- 4. The user can now
--
--     * logout and sign in
--     * reset their password
--
-- = Using JSON Endpoints
--
-- We are assuming that you have declared auth route as follows
-- 
-- @
--    /auth AuthR Auth getAuth
-- @
-- 
-- If you are using a different route, then you have to adjust the
-- endpoints accordingly.
--
--     * Registration
-- 
-- @
--       Endpoint: \/auth\/page\/email\/register
--       Method: POST
--       JSON Data: {
--                      "email": "myemail@domain.com",
--                      "password": "myStrongPassword" (optional)
--                  }
-- @
-- 
--     * Forgot password
--  
-- @
--       Endpoint: \/auth\/page\/email\/forgot-password
--       Method: POST
--       JSON Data: { "email": "myemail@domain.com" }
-- @
--
--     * Login
--  
-- @
--       Endpoint: \/auth\/page\/email\/login
--       Method: POST
--       JSON Data: { 
--                      "email": "myemail@domain.com",
--                      "password": "myStrongPassword"
--                  }
-- @
-- 
--     * Set new password
--
-- @
--       Endpoint: \/auth\/page\/email\/set-password
--       Method: POST
--       JSON Data: {
--                       "new": "newPassword",
--                       "confirm": "newPassword",
--                       "current": "currentPassword"
--                  }
-- @
--
--  Note that in the set password endpoint, the presence of the key
--  "current" is dependent on how the 'needOldPassword' is defined in
--  the instance for 'YesodAuthEmail'.

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
    , verifyR
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
     -- * Default handlers
    , defaultEmailLoginHandler
    , defaultRegisterHandler
    , defaultForgotPasswordHandler
    , defaultSetPasswordHandler
    ) where

import           Yesod.Auth
import qualified Yesod.Auth.Message       as Msg
import           Yesod.Core
import           Yesod.Form
import qualified Yesod.Auth.Util.PasswordStore as PS
import           Control.Applicative      ((<$>), (<*>))
import qualified Crypto.Hash              as H
import qualified Crypto.Nonce             as Nonce
import           Data.ByteString.Base16   as B16
import           Data.Text                (Text)
import qualified Data.Text                as TS
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import qualified Data.Text.Encoding       as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Time                (addUTCTime, getCurrentTime)
import           Safe                     (readMay)
import           System.IO.Unsafe         (unsafePerformIO)
import qualified Text.Email.Validate
import           Data.Aeson.Types (Parser, Result(..), parseMaybe, withObject, (.:?))
import           Data.Maybe (isJust)
import Data.ByteArray (convert)

loginR, registerR, forgotPasswordR, setpassR :: AuthRoute
loginR = PluginR "email" ["login"]
registerR = PluginR "email" ["register"]
forgotPasswordR = PluginR "email" ["forgot-password"]
setpassR = PluginR "email" ["set-password"]

verifyURLHasSetPassText :: Text
verifyURLHasSetPassText = "has-set-pass"

-- |
--
-- @since 1.4.5
verifyR :: Text -> Text -> Bool -> AuthRoute -- FIXME
verifyR eid verkey hasSetPass = PluginR "email" path
    where path = "verify":eid:verkey:(if hasSetPass then [verifyURLHasSetPassText] else [])

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
-- @since 1.2.0
type Identifier = Text

-- | Data stored in a database for each e-mail address.
data EmailCreds site = EmailCreds
    { emailCredsId     :: AuthEmailId site
    , emailCredsAuthId :: Maybe (AuthId site)
    , emailCredsStatus :: VerStatus
    , emailCredsVerkey :: Maybe VerKey
    , emailCredsEmail  :: Email
    }

data ForgotPasswordForm = ForgotPasswordForm { _forgotEmail :: Text }
data PasswordForm = PasswordForm { _passwordCurrent :: Text, _passwordNew :: Text, _passwordConfirm :: Text }
data UserForm = UserForm { _userFormEmail :: Text }
data UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }

class ( YesodAuth site
      , PathPiece (AuthEmailId site)
      , (RenderMessage site Msg.AuthMessage)
      )
  => YesodAuthEmail site where
    type AuthEmailId site

    -- | Add a new email address to the database, but indicate that the address
    -- has not yet been verified.
    --
    -- @since 1.1.0
    addUnverified :: Email -> VerKey -> AuthHandler site (AuthEmailId site)

    -- | Similar to `addUnverified`, but comes with the registered password
    -- the default implementation is just `addUnverified`, which ignores the password
    -- you may override this to save the salted password to your database
    --
    -- @since 1.6.4
    addUnverifiedWithPass :: Email -> VerKey -> SaltedPass -> AuthHandler site (AuthEmailId site)
    addUnverifiedWithPass email verkey _ = addUnverified email verkey

    -- | Send an email to the given address to verify ownership.
    --
    -- @since 1.1.0
    sendVerifyEmail :: Email -> VerKey -> VerUrl -> AuthHandler site ()

    -- | Get the verification key for the given email ID.
    --
    -- @since 1.1.0
    getVerifyKey :: AuthEmailId site -> AuthHandler site (Maybe VerKey)

    -- | Set the verification key for the given email ID.
    --
    -- @since 1.1.0
    setVerifyKey :: AuthEmailId site -> VerKey -> AuthHandler site ()

    -- | Hash and salt a password
    --
    -- Default: 'saltPass'.
    --
    -- @since 1.4.20
    hashAndSaltPassword :: Text -> AuthHandler site SaltedPass
    hashAndSaltPassword = liftIO . saltPass

    -- | Verify a password matches the stored password for the given account.
    --
    -- Default: Fetch a password with 'getPassword' and match using 'Yesod.Auth.Util.PasswordStore.verifyPassword'.
    --
    -- @since 1.4.20
    verifyPassword :: Text -> SaltedPass -> AuthHandler site Bool
    verifyPassword plain salted = return $ isValidPass plain salted

    -- | Verify the email address on the given account.
    --
    -- __/Warning!/__ If you have persisted the @'AuthEmailId' site@
    -- somewhere, this method should delete that key, or make it unusable
    -- in some fashion. Otherwise, the same key can be used multiple times!
    --
    -- See <https://github.com/yesodweb/yesod/issues/1222>.
    --
    -- @since 1.1.0
    verifyAccount :: AuthEmailId site -> AuthHandler site (Maybe (AuthId site))

    -- | Get the salted password for the given account.
    --
    -- @since 1.1.0
    getPassword :: AuthId site -> AuthHandler site (Maybe SaltedPass)

    -- | Set the salted password for the given account.
    --
    -- @since 1.1.0
    setPassword :: AuthId site -> SaltedPass -> AuthHandler site ()

    -- | Get the credentials for the given @Identifier@, which may be either an
    -- email address or some other identification (e.g., username).
    --
    -- @since 1.2.0
    getEmailCreds :: Identifier -> AuthHandler site (Maybe (EmailCreds site))

    -- | Get the email address for the given email ID.
    --
    -- @since 1.1.0
    getEmail :: AuthEmailId site -> AuthHandler site (Maybe Email)

    -- | Generate a random alphanumeric string.
    --
    -- @since 1.1.0
    randomKey :: site -> IO VerKey
    randomKey _ = Nonce.nonce128urlT defaultNonceGen

    -- | Route to send user to after password has been set correctly.
    --
    -- @since 1.2.0
    afterPasswordRoute :: site -> Route site

    -- | Route to send user to after verification with a password
    --
    -- @since 1.6.4
    afterVerificationWithPass :: site -> Route site
    afterVerificationWithPass = afterPasswordRoute

    -- | Does the user need to provide the current password in order to set a
    -- new password?
    --
    -- Default: if the user logged in via an email link do not require a password.
    --
    -- @since 1.2.1
    needOldPassword :: AuthId site -> AuthHandler site Bool
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
    checkPasswordSecurity :: AuthId site -> Text -> AuthHandler site (Either Text ())
    checkPasswordSecurity _ x
        | TS.length x >= 3 = return $ Right ()
        | otherwise = return $ Left "Password must be at least three characters"

    -- | Response after sending a confirmation email.
    --
    -- @since 1.2.2
    confirmationEmailSentResponse :: Text -> AuthHandler site TypedContent
    confirmationEmailSentResponse identifier = do
        mr <- getMessageRender
        selectRep $ do
            provideJsonMessage (mr msg)
            provideRep $ authLayout $ do
              setTitleI Msg.ConfirmationEmailSentTitle
              [whamlet|<p>_{msg}|]
      where
        msg = Msg.ConfirmationEmailSent identifier

    -- | If a response is set, it will be used when an already-verified email
    -- tries to re-register. Otherwise, `confirmationEmailSentResponse` will be
    -- used.
    --
    -- @since 1.6.4
    emailPreviouslyRegisteredResponse :: MonadAuthHandler site m => Text -> Maybe (m TypedContent)
    emailPreviouslyRegisteredResponse _ = Nothing

    -- | Additional normalization of email addresses, besides standard canonicalization.
    --
    -- Default: Lower case the email address.
    --
    -- @since 1.2.3
    normalizeEmailAddress :: site -> Text -> Text
    normalizeEmailAddress _ = TS.toLower

    -- | Handler called to render the login page.
    -- The default works fine, but you may want to override it in
    -- order to have a different DOM.
    --
    -- Default: 'defaultEmailLoginHandler'.
    --
    -- @since 1.4.17
    emailLoginHandler :: (Route Auth -> Route site) -> WidgetFor site ()
    emailLoginHandler = defaultEmailLoginHandler


    -- | Handler called to render the registration page.  The
    -- default works fine, but you may want to override it in
    -- order to have a different DOM.
    --
    -- Default: 'defaultRegisterHandler'.
    --
    -- @since: 1.2.6
    registerHandler :: AuthHandler site Html
    registerHandler = defaultRegisterHandler

    -- | Handler called to render the \"forgot password\" page.
    -- The default works fine, but you may want to override it in
    -- order to have a different DOM.
    --
    -- Default: 'defaultForgotPasswordHandler'.
    --
    -- @since: 1.2.6
    forgotPasswordHandler :: AuthHandler site Html
    forgotPasswordHandler = defaultForgotPasswordHandler

    -- | Handler called to render the \"set password\" page.  The
    -- default works fine, but you may want to override it in
    -- order to have a different DOM.
    --
    -- Default: 'defaultSetPasswordHandler'.
    --
    -- @since: 1.2.6
    setPasswordHandler ::
         Bool
         -- ^ Whether the old password is needed.  If @True@, a
         -- field for the old password should be presented.
         -- Otherwise, just two fields for the new password are
         -- needed.
      -> AuthHandler site TypedContent
    setPasswordHandler = defaultSetPasswordHandler

authEmail :: (YesodAuthEmail m) => AuthPlugin m
authEmail =
    AuthPlugin "email" dispatch emailLoginHandler
  where
    dispatch "GET" ["register"] = getRegisterR >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR >>= sendResponse
    dispatch "GET" ["forgot-password"] = getForgotPasswordR >>= sendResponse
    dispatch "POST" ["forgot-password"] = postForgotPasswordR >>= sendResponse
    dispatch "GET" ["verify", eid, verkey] =
        case fromPathPiece eid of
            Nothing -> notFound
            Just eid' -> getVerifyR eid' verkey False >>= sendResponse
    dispatch "GET" ["verify", eid, verkey, hasSetPass] =
        case fromPathPiece eid of
            Nothing -> notFound
            Just eid' -> getVerifyR eid' verkey (hasSetPass == verifyURLHasSetPassText) >>= sendResponse
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["set-password"] = getPasswordR >>= sendResponse
    dispatch "POST" ["set-password"] = postPasswordR >>= sendResponse
    dispatch _ _ = notFound

getRegisterR :: YesodAuthEmail master => AuthHandler master Html
getRegisterR = registerHandler

-- | Default implementation of 'emailLoginHandler'.
--
-- @since 1.4.17
defaultEmailLoginHandler
  :: YesodAuthEmail master
  => (Route Auth -> Route master)
  -> WidgetFor master ()
defaultEmailLoginHandler toParent = do
        (widget, enctype) <- generateFormPost loginForm

        [whamlet|
            <form method="post" action="@{toParent loginR}", enctype=#{enctype}>
                <div id="emailLoginForm">
                    ^{widget}
                    <div>
                        <button type=submit .btn .btn-success>
                            _{Msg.LoginViaEmail}
                        &nbsp;
                        <a href="@{toParent registerR}" .btn .btn-default>
                            _{Msg.RegisterLong}
        |]
  where
    loginForm extra = do

        emailMsg <- renderMessage' Msg.Email
        (emailRes, emailView) <- mreq emailField (emailSettings emailMsg) Nothing

        passwordMsg <- renderMessage' Msg.Password
        (passwordRes, passwordView) <- mreq passwordField (passwordSettings passwordMsg) Nothing

        let userRes = UserLoginForm Control.Applicative.<$> emailRes
                                    Control.Applicative.<*> passwordRes
        let widget = do
            [whamlet|
                #{extra}
                <div>
                    ^{fvInput emailView}
                <div>
                    ^{fvInput passwordView}
            |]

        return (userRes, widget)
    emailSettings emailMsg = do
        FieldSettings {
            fsLabel = SomeMessage Msg.Email,
            fsTooltip = Nothing,
            fsId = Just "email",
            fsName = Just "email",
            fsAttrs = [("autofocus", ""), ("placeholder", emailMsg)]
        }
    passwordSettings passwordMsg =
         FieldSettings {
            fsLabel = SomeMessage Msg.Password,
            fsTooltip = Nothing,
            fsId = Just "password",
            fsName = Just "password",
            fsAttrs = [("placeholder", passwordMsg)]
        }
    renderMessage' msg = do
        langs <- languages
        master <- getYesod
        return $ renderAuthMessage master langs msg

-- | Default implementation of 'registerHandler'.
--
-- @since 1.2.6
defaultRegisterHandler :: YesodAuthEmail master => AuthHandler master Html
defaultRegisterHandler = do
    (widget, enctype) <- generateFormPost registrationForm
    toParentRoute <- getRouteToParent
    authLayout $ do
        setTitleI Msg.RegisterLong
        [whamlet|
            <p>_{Msg.EnterEmail}
            <form method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
                <div id="registerForm">
                    ^{widget}
                <button .btn>_{Msg.Register}
        |]
    where
        registrationForm extra = do
            let emailSettings = FieldSettings {
                fsLabel = SomeMessage Msg.Email,
                fsTooltip = Nothing,
                fsId = Just "email",
                fsName = Just "email",
                fsAttrs = [("autofocus", "")]
            }

            (emailRes, emailView) <- mreq emailField emailSettings Nothing

            let userRes = UserForm <$> emailRes
            let widget = do
                [whamlet|
                    #{extra}
                    ^{fvLabel emailView}
                    ^{fvInput emailView}
                |]

            return (userRes, widget)

parseRegister :: Value -> Parser (Text, Maybe Text)
parseRegister = withObject "email" (\obj -> do
                                      email <- obj .: "email"
                                      pass <- obj .:? "password"
                                      return (email, pass))

registerHelper :: YesodAuthEmail master
               => Bool -- ^ allow usernames?
               -> Bool -- ^ forgot password?
               -> Route Auth
               -> AuthHandler master TypedContent
registerHelper allowUsername forgotPassword dest = do
    y <- getYesod
    checkCsrfHeaderOrParam defaultCsrfHeaderName defaultCsrfParamName
    result <- runInputPostResult $ (,)
        <$> ireq textField "email"
        <*> iopt textField "password"

    creds <- case result of
                 FormSuccess (iden, pass) -> return $ Just (iden, pass)
                 _ -> do
                     (creds :: Result Value) <- parseCheckJsonBody
                     return $ case creds of
                                  Error _ -> Nothing
                                  Success val -> parseMaybe parseRegister val

    let eidentifier = case creds of
                          Nothing -> Left Msg.NoIdentifierProvided
                          Just (x, _)
                              | Just x' <- Text.Email.Validate.canonicalizeEmail (encodeUtf8 x) ->
                                         Right $ normalizeEmailAddress y $ decodeUtf8With lenientDecode x'
                              | allowUsername -> Right $ TS.strip x
                              | otherwise -> Left Msg.InvalidEmailAddress

    let mpass = case (forgotPassword, creds) of
                    (False, Just (_, mp)) -> mp
                    _ -> Nothing

    case eidentifier of
      Left route -> loginErrorMessageI dest route
      Right identifier -> do
            mecreds <- getEmailCreds identifier
            registerCreds <-
                case mecreds of
                    Just (EmailCreds lid _ verStatus (Just key) email) -> return $ Just (lid, verStatus, key, email)
                    Just (EmailCreds lid _ verStatus Nothing email) -> do
                        key <- liftIO $ randomKey y
                        setVerifyKey lid key
                        return $ Just (lid, verStatus, key, email)
                    Nothing
                        | allowUsername -> return Nothing
                        | otherwise -> do
                            key <- liftIO $ randomKey y
                            lid <- case mpass of
                                Just pass -> do
                                    salted <- hashAndSaltPassword pass
                                    addUnverifiedWithPass identifier key salted
                                _ -> addUnverified identifier key
                            return $ Just (lid, False, key, identifier)
            case registerCreds of
                Nothing -> loginErrorMessageI dest (Msg.IdentifierNotFound identifier)
                Just creds@(_, False, _, _) -> sendConfirmationEmail creds
                Just creds@(_, True, _, _) -> do
                  if forgotPassword then sendConfirmationEmail creds
                    else case emailPreviouslyRegisteredResponse identifier of
                      Just response -> response
                      Nothing -> sendConfirmationEmail creds
              where sendConfirmationEmail (lid, _, verKey, email) = do
                      render <- getUrlRender
                      tp <- getRouteToParent
                      let verUrl = render $ tp $ verifyR (toPathPiece lid) verKey (isJust mpass)
                      sendVerifyEmail email verKey verUrl
                      confirmationEmailSentResponse identifier


postRegisterR :: YesodAuthEmail master => AuthHandler master TypedContent
postRegisterR = registerHelper False False registerR

getForgotPasswordR :: YesodAuthEmail master => AuthHandler master Html
getForgotPasswordR = forgotPasswordHandler

-- | Default implementation of 'forgotPasswordHandler'.
--
-- @since 1.2.6
defaultForgotPasswordHandler :: YesodAuthEmail master => AuthHandler master Html
defaultForgotPasswordHandler = do
    (widget, enctype) <- generateFormPost forgotPasswordForm
    toParent <- getRouteToParent
    authLayout $ do
        setTitleI Msg.PasswordResetTitle
        [whamlet|
            <p>_{Msg.PasswordResetPrompt}
            <form method=post action=@{toParent forgotPasswordR} enctype=#{enctype}>
                <div id="forgotPasswordForm">
                    ^{widget}
                    <button .btn>_{Msg.SendPasswordResetEmail}
        |]
  where
    forgotPasswordForm extra = do
        (emailRes, emailView) <- mreq emailField emailSettings Nothing

        let forgotPasswordRes = ForgotPasswordForm <$> emailRes
        let widget = do
            [whamlet|
                #{extra}
                ^{fvLabel emailView}
                ^{fvInput emailView}
            |]
        return (forgotPasswordRes, widget)

    emailSettings =
        FieldSettings {
            fsLabel = SomeMessage Msg.ProvideIdentifier,
            fsTooltip = Nothing,
            fsId = Just "forgotPassword",
            fsName = Just "email",
            fsAttrs = [("autofocus", "")]
        }

postForgotPasswordR :: YesodAuthEmail master => AuthHandler master TypedContent
postForgotPasswordR = registerHelper True True forgotPasswordR

getVerifyR :: YesodAuthEmail site
           => AuthEmailId site
           -> Text
           -> Bool
           -> AuthHandler site TypedContent
getVerifyR lid key hasSetPass = do
    realKey <- getVerifyKey lid
    memail <- getEmail lid
    mr <- getMessageRender
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            muid <- verifyAccount lid
            case muid of
                Nothing -> invalidKey mr
                Just uid -> do
                    setCreds False $ Creds "email-verify" email [("verifiedEmail", email)] -- FIXME uid?
                    setLoginLinkKey uid
                    let msgAv = Msg.AddressVerified
                    selectRep $ do
                      provideRep $ do
                        addMessageI "success" msgAv
                        redirectRoute <- if hasSetPass
                            then do
                              y <- getYesod
                              return $ afterVerificationWithPass y
                            else do
                              tp <- getRouteToParent
                              return $ tp setpassR
                        fmap asHtml $ redirect redirectRoute
                      provideJsonMessage $ mr msgAv
        _ -> invalidKey mr
  where
    msgIk = Msg.InvalidKey
    invalidKey mr = messageJson401 (mr msgIk) $ authLayout $ do
        setTitleI msgIk
        [whamlet|
$newline never
<p>_{msgIk}
|]


parseCreds :: Value -> Parser (Text, Text)
parseCreds = withObject "creds" (\obj -> do
                                   email' <- obj .: "email"
                                   pass <- obj .: "password"
                                   return (email', pass))


postLoginR :: YesodAuthEmail master => AuthHandler master TypedContent
postLoginR = do
    result <- runInputPostResult $ (,)
        <$> ireq textField "email"
        <*> ireq textField "password"

    midentifier <- case result of
                     FormSuccess (iden, pass) -> return $ Just (iden, pass)
                     _ -> do
                       (creds :: Result Value) <- parseCheckJsonBody
                       case creds of
                         Error _ -> return Nothing
                         Success val -> return $ parseMaybe parseCreds val

    case midentifier of
      Nothing -> loginErrorMessageI LoginR Msg.NoIdentifierProvided
      Just (identifier, pass) -> do
          mecreds <- getEmailCreds identifier
          maid <-
              case ( mecreds >>= emailCredsAuthId
                   , emailCredsEmail <$> mecreds
                   , emailCredsStatus <$> mecreds
                   ) of
                (Just aid, Just email', Just True) -> do
                      mrealpass <- getPassword aid
                      case mrealpass of
                        Nothing -> return Nothing
                        Just realpass -> do
                            passValid <- verifyPassword pass realpass
                            return $ if passValid
                                    then Just email'
                                    else Nothing
                _ -> return Nothing
          let isEmail = Text.Email.Validate.isValid $ encodeUtf8 identifier
          case maid of
            Just email' ->
                setCredsRedirect $ Creds
                         (if isEmail then "email" else "username")
                         email'
                         [("verifiedEmail", email')]
            Nothing ->
                loginErrorMessageI LoginR $
                                   if isEmail
                                   then Msg.InvalidEmailPass
                                   else Msg.InvalidUsernamePass

getPasswordR :: YesodAuthEmail master => AuthHandler master TypedContent
getPasswordR = do
    maid <- maybeAuthId
    case maid of
        Nothing -> loginErrorMessageI LoginR Msg.BadSetPass
        Just _ -> do
            needOld <- maybe (return True) needOldPassword maid
            setPasswordHandler needOld

-- | Default implementation of 'setPasswordHandler'.
--
-- @since 1.2.6
defaultSetPasswordHandler :: YesodAuthEmail master => Bool -> AuthHandler master TypedContent
defaultSetPasswordHandler needOld = do
    messageRender <- getMessageRender
    toParent <- getRouteToParent
    selectRep $ do
        provideJsonMessage $ messageRender Msg.SetPass
        provideRep $ authLayout $ do
            (widget, enctype) <- generateFormPost setPasswordForm
            setTitleI Msg.SetPassTitle
            [whamlet|
                <h3>_{Msg.SetPass}
                <form method="post" action="@{toParent setpassR}" enctype=#{enctype}>
                    ^{widget}
            |]
  where
    setPasswordForm extra = do
        (currentPasswordRes, currentPasswordView) <- mreq passwordField currentPasswordSettings Nothing
        (newPasswordRes, newPasswordView) <- mreq passwordField newPasswordSettings Nothing
        (confirmPasswordRes, confirmPasswordView) <- mreq passwordField confirmPasswordSettings Nothing

        let passwordFormRes = PasswordForm <$> currentPasswordRes <*> newPasswordRes <*> confirmPasswordRes
        let widget = do
            [whamlet|
                #{extra}
                <table>
                    $if needOld
                        <tr>
                            <th>
                                ^{fvLabel currentPasswordView}
                            <td>
                                ^{fvInput currentPasswordView}
                    <tr>
                        <th>
                            ^{fvLabel newPasswordView}
                        <td>
                            ^{fvInput newPasswordView}
                    <tr>
                        <th>
                            ^{fvLabel confirmPasswordView}
                        <td>
                            ^{fvInput confirmPasswordView}
                    <tr>
                        <td colspan="2">
                            <input type=submit value=_{Msg.SetPassTitle}>
            |]

        return (passwordFormRes, widget)
    currentPasswordSettings =
         FieldSettings {
             fsLabel = SomeMessage Msg.CurrentPassword,
             fsTooltip = Nothing,
             fsId = Just "currentPassword",
             fsName = Just "current",
             fsAttrs = [("autofocus", "")]
         }
    newPasswordSettings =
        FieldSettings {
            fsLabel = SomeMessage Msg.NewPass,
            fsTooltip = Nothing,
            fsId = Just "newPassword",
            fsName = Just "new",
            fsAttrs = [("autofocus", ""), (":not", ""), ("needOld:autofocus", "")]
        }
    confirmPasswordSettings =
        FieldSettings {
            fsLabel = SomeMessage Msg.ConfirmPass,
            fsTooltip = Nothing,
            fsId = Just "confirmPassword",
            fsName = Just "confirm",
            fsAttrs = [("autofocus", "")]
        }

parsePassword :: Value -> Parser (Text, Text, Maybe Text)
parsePassword = withObject "password" (\obj -> do
                                         email' <- obj .: "new"
                                         pass <- obj .: "confirm"
                                         curr <- obj .:? "current"
                                         return (email', pass, curr))

postPasswordR :: YesodAuthEmail master => AuthHandler master TypedContent
postPasswordR = do
    maid <- maybeAuthId
    (creds :: Result Value) <- parseCheckJsonBody
    let jcreds = case creds of
                   Error _ -> Nothing
                   Success val -> parseMaybe parsePassword val
    let doJsonParsing = isJust jcreds
    case maid of
        Nothing -> loginErrorMessageI LoginR Msg.BadSetPass
        Just aid -> do
            tm <- getRouteToParent
            needOld <- needOldPassword aid
            if not needOld then confirmPassword aid tm jcreds else do
                res <- runInputPostResult $ ireq textField "current"
                let fcurrent = case res of
                                 FormSuccess currentPass -> Just currentPass
                                 _ -> Nothing
                let current = if doJsonParsing
                              then getThird jcreds
                              else fcurrent
                mrealpass <- getPassword aid
                case (mrealpass, current) of
                    (Nothing, _) ->
                        loginErrorMessage (tm setpassR) "You do not currently have a password set on your account"
                    (_, Nothing) ->
                        loginErrorMessageI LoginR Msg.BadSetPass
                    (Just realpass, Just current') -> do
                        passValid <- verifyPassword current' realpass
                        if passValid
                          then confirmPassword aid tm jcreds
                          else loginErrorMessage (tm setpassR) "Invalid current password, please try again"

  where
    msgOk = Msg.PassUpdated
    getThird (Just (_,_,t)) = t
    getThird Nothing = Nothing
    getNewConfirm (Just (a,b,_)) = Just (a,b)
    getNewConfirm _ = Nothing
    confirmPassword aid tm jcreds = do
        res <- runInputPostResult $ (,)
            <$> ireq textField "new"
            <*> ireq textField "confirm"
        let creds = if (isJust jcreds)
                    then getNewConfirm jcreds
                    else case res of
                           FormSuccess res' -> Just res'
                           _ -> Nothing
        case creds of
          Nothing -> loginErrorMessageI setpassR Msg.PassMismatch
          Just (new, confirm) ->
              if new /= confirm
              then loginErrorMessageI setpassR Msg.PassMismatch
              else do
                isSecure <- checkPasswordSecurity aid new
                case isSecure of
                  Left e -> loginErrorMessage (tm setpassR) e
                  Right () -> do
                     salted <- hashAndSaltPassword new
                     y <- do
                                setPassword aid salted
                                deleteSession loginLinkKey
                                addMessageI "success" msgOk
                                getYesod

                     mr <- getMessageRender
                     selectRep $ do
                         provideRep $ 
                            fmap asHtml $ redirect $ afterPasswordRoute y
                         provideJsonMessage (mr msgOk)

saltLength :: Int
saltLength = 5

-- | Salt a password with a randomly generated salt.
saltPass :: Text -> IO Text
saltPass = fmap (decodeUtf8With lenientDecode)
         . flip PS.makePassword 16
         . encodeUtf8

saltPass' :: String -> String -> String
saltPass' salt pass =
    salt ++ T.unpack (TE.decodeUtf8 $ B16.encode $ convert (H.hash (TE.encodeUtf8 $ T.pack $ salt ++ pass) :: H.Digest H.MD5))

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
-- @since 1.2.1
loginLinkKey :: Text
loginLinkKey = "_AUTH_EMAIL_LOGIN_LINK"

-- | Set 'loginLinkKey' to the current time.
--
-- @since 1.2.1
--setLoginLinkKey :: (MonadHandler m) => AuthId site -> m ()
setLoginLinkKey :: (MonadHandler m, YesodAuthEmail (HandlerSite m))
                => AuthId (HandlerSite m)
                -> m ()
setLoginLinkKey aid = do
    now <- liftIO getCurrentTime
    setSession loginLinkKey $ TS.pack $ show (toPathPiece aid, now)

-- See https://github.com/yesodweb/yesod/issues/1245 for discussion on this
-- use of unsafePerformIO.
defaultNonceGen :: Nonce.Generator
defaultNonceGen = unsafePerformIO (Nonce.new)
{-# NOINLINE defaultNonceGen #-}
