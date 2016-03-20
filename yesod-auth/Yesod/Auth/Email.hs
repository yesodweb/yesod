{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE PatternGuards           #-}
{-# LANGUAGE QuasiQuotes             #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE TypeFamilies            #-}
-- | A Yesod plugin for Authentication via e-mail
--
-- This plugin works out of the box by only setting a few methods on the type class
-- that tell the plugin how to interoprate with your user data storage (your database).
-- However, almost everything is customizeable by setting more methods on the type class.
-- In addition, you can send all the form submissions via JSON and completely control the user's flow.
-- This is a standard registration e-mail flow
--
-- 1) A user registers a new e-mail address, and an e-mail is sent there
-- 2) The user clicks on the registration link in the e-mail
--    Note that at this point they are actually logged in (without a password)
--    That means that when they log out they will need to reset their password
-- 3) The user sets their password and is redirected to the site.
-- 4) The user can now
--    * logout and sign in
--    * reset their password
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
    , defaultRegisterHandler
    , defaultForgotPasswordHandler
    , defaultSetPasswordHandler
    ) where

import           Yesod.Auth
import qualified Yesod.Auth.Message       as Msg
import           Yesod.Core
import           Yesod.Form
import qualified Yesod.PasswordStore      as PS

import           Control.Applicative      ((<$>), (<*>))
import qualified Crypto.Hash.MD5          as H
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


loginR, registerR, forgotPasswordR, setpassR :: AuthRoute
loginR = PluginR "email" ["login"]
registerR = PluginR "email" ["register"]
forgotPasswordR = PluginR "email" ["forgot-password"]
setpassR = PluginR "email" ["set-password"]

-- |
--
-- Since 1.4.5
verifyR :: Text -> Text -> AuthRoute -- FIXME
verifyR eid verkey = PluginR "email" ["verify", eid, verkey]

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
    { emailCredsId     :: AuthEmailId site
    , emailCredsAuthId :: Maybe (AuthId site)
    , emailCredsStatus :: VerStatus
    , emailCredsVerkey :: Maybe VerKey
    , emailCredsEmail  :: Email
    }

data ForgotPasswordForm = ForgotPasswordForm { forgotEmail :: Text }
data PasswordForm = PasswordForm { passwordCurrent :: Text, passwordNew :: Text, passwordConfirm :: Text }
data UserForm = UserForm { email :: Text }
data UserLoginForm = UserLoginForm { loginEmail :: Text, loginPassword :: Text }

class ( YesodAuth site
      , PathPiece (AuthEmailId site)
      , (RenderMessage site Msg.AuthMessage)
      )
  => YesodAuthEmail site where
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
    randomKey _ = Nonce.nonce128urlT defaultNonceGen

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
    confirmationEmailSentResponse :: Text -> HandlerT site IO TypedContent
    confirmationEmailSentResponse identifier = do
        mr <- getMessageRender
        selectRep $ do
            provideJsonMessage (mr msg)
            provideRep $ authLayout $ do
              setTitleI Msg.ConfirmationEmailSentTitle
              [whamlet|<p>_{msg}|]
      where
        msg = Msg.ConfirmationEmailSent identifier

    -- | Additional normalization of email addresses, besides standard canonicalization.
    --
    -- Default: Lower case the email address.
    --
    -- Since 1.2.3
    normalizeEmailAddress :: site -> Text -> Text
    normalizeEmailAddress _ = TS.toLower

    -- | Handler called to render the registration page.  The
    -- default works fine, but you may want to override it in
    -- order to have a different DOM.
    --
    -- Default: 'defaultRegisterHandler'.
    --
    -- Since: 1.2.6.
    registerHandler :: AuthHandler site Html
    registerHandler = defaultRegisterHandler

    -- | Handler called to render the \"forgot password\" page.
    -- The default works fine, but you may want to override it in
    -- order to have a different DOM.
    --
    -- Default: 'defaultForgotPasswordHandler'.
    --
    -- Since: 1.2.6.
    forgotPasswordHandler :: AuthHandler site Html
    forgotPasswordHandler = defaultForgotPasswordHandler

    -- | Handler called to render the \"set password\" page.  The
    -- default works fine, but you may want to override it in
    -- order to have a different DOM.
    --
    -- Default: 'defaultSetPasswordHandler'.
    --
    -- Since: 1.2.6.
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
            Just eid' -> getVerifyR eid' verkey >>= sendResponse
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["set-password"] = getPasswordR >>= sendResponse
    dispatch "POST" ["set-password"] = postPasswordR >>= sendResponse
    dispatch _ _ = notFound

getRegisterR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
getRegisterR = registerHandler

emailLoginHandler :: YesodAuthEmail master => (Route Auth -> Route master) -> WidgetT master IO ()
emailLoginHandler toParent = do
        (widget, enctype) <- liftWidgetT $ generateFormPost loginForm

        [whamlet|
            <form method="post" action="@{toParent loginR}">
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

        let userRes = UserLoginForm <$> emailRes <*> passwordRes
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
-- Since: 1.2.6
defaultRegisterHandler :: YesodAuthEmail master => AuthHandler master Html
defaultRegisterHandler = do
    (widget, enctype) <- lift $ generateFormPost registrationForm
    toParentRoute <- getRouteToParent
    lift $ authLayout $ do
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

registerHelper :: YesodAuthEmail master
               => Bool -- ^ allow usernames?
               -> Route Auth
               -> HandlerT Auth (HandlerT master IO) TypedContent
registerHelper allowUsername dest = do
    y <- lift getYesod
    midentifier <- lookupPostParam "email"
    let eidentifier = case midentifier of
            Nothing -> Left Msg.NoIdentifierProvided
            Just x
                | Just x' <- Text.Email.Validate.canonicalizeEmail (encodeUtf8 x) ->
                    Right $ normalizeEmailAddress y $ decodeUtf8With lenientDecode x'
                | allowUsername -> Right $ TS.strip x
                | otherwise -> Left Msg.InvalidEmailAddress

    case eidentifier of
        Left route -> loginErrorMessageI dest route
        Right identifier -> do

            mecreds <- lift $ getEmailCreds identifier
            registerCreds <-
                case mecreds of
                    Just (EmailCreds lid _ _ (Just key) email) -> return $ Just (lid, key, email)
                    Just (EmailCreds lid _ _ Nothing email) -> do
                        key <- liftIO $ randomKey y
                        lift $ setVerifyKey lid key
                        return $ Just (lid, key, email)
                    Nothing
                        | allowUsername -> return Nothing
                        | otherwise -> do
                            key <- liftIO $ randomKey y
                            lid <- lift $ addUnverified identifier key
                            return $ Just (lid, key, identifier)

            case registerCreds of
                Nothing -> loginErrorMessageI dest (Msg.IdentifierNotFound identifier)
                Just (lid, verKey, email) -> do
                    render <- getUrlRender
                    let verUrl = render $ verifyR (toPathPiece lid) verKey
                    lift $ sendVerifyEmail email verKey verUrl
                    lift $ confirmationEmailSentResponse identifier

postRegisterR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) TypedContent
postRegisterR = registerHelper False registerR

getForgotPasswordR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
getForgotPasswordR = forgotPasswordHandler

-- | Default implementation of 'forgotPasswordHandler'.
--
-- Since: 1.2.6
defaultForgotPasswordHandler :: YesodAuthEmail master => AuthHandler master Html
defaultForgotPasswordHandler = do
    (widget, enctype) <- lift $ generateFormPost forgotPasswordForm
    toParent <- getRouteToParent
    lift $ authLayout $ do
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

postForgotPasswordR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) TypedContent
postForgotPasswordR = registerHelper True forgotPasswordR

getVerifyR :: YesodAuthEmail site
           => AuthEmailId site
           -> Text
           -> HandlerT Auth (HandlerT site IO) TypedContent
getVerifyR lid key = do
    realKey <- lift $ getVerifyKey lid
    memail <- lift $ getEmail lid
    mr <- lift getMessageRender
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            muid <- lift $ verifyAccount lid
            case muid of
                Nothing -> invalidKey mr
                Just uid -> do
                    lift $ setCreds False $ Creds "email-verify" email [("verifiedEmail", email)] -- FIXME uid?
                    lift $ setLoginLinkKey uid
                    let msgAv = Msg.AddressVerified
                    selectRep $ do
                      provideRep $ do
                        lift $ addMessageI "success" msgAv
                        fmap asHtml $ redirect setpassR
                      provideJsonMessage $ mr msgAv
        _ -> invalidKey mr
  where
    msgIk = Msg.InvalidKey
    invalidKey mr = messageJson401 (mr msgIk) $ lift $ authLayout $ do
        setTitleI msgIk
        [whamlet|
$newline never
<p>_{msgIk}
|]


postLoginR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) TypedContent
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
            lift $ setCredsRedirect $ Creds
                (if isEmail then "email" else "username")
                email
                [("verifiedEmail", email)]
        Nothing ->
            loginErrorMessageI LoginR $
                if isEmail
                    then Msg.InvalidEmailPass
                    else Msg.InvalidUsernamePass

getPasswordR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) TypedContent
getPasswordR = do
    maid <- lift maybeAuthId
    case maid of
        Nothing -> loginErrorMessageI LoginR Msg.BadSetPass
        Just _ -> do
            needOld <- maybe (return True) (lift . needOldPassword) maid
            setPasswordHandler needOld

-- | Default implementation of 'setPasswordHandler'.
--
-- Since: 1.2.6
defaultSetPasswordHandler :: YesodAuthEmail master => Bool -> AuthHandler master TypedContent
defaultSetPasswordHandler needOld = do
    messageRender <- lift getMessageRender
    toParent <- getRouteToParent
    selectRep $ do
        provideJsonMessage $ messageRender Msg.SetPass
        provideRep $ lift $ authLayout $ do
            (widget, enctype) <- liftWidgetT $ generateFormPost $ setPasswordForm needOld
            setTitleI Msg.SetPassTitle
            [whamlet|
                <h3>_{Msg.SetPass}
                <form method="post" action="@{toParent setpassR}">
                    ^{widget}
            |]
  where
    setPasswordForm needOld extra = do
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



postPasswordR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) TypedContent
postPasswordR = do
    maid <- lift maybeAuthId
    case maid of
        Nothing -> loginErrorMessageI LoginR Msg.BadSetPass
        Just aid -> do
            tm <- getRouteToParent

            needOld <- lift $ needOldPassword aid
            if not needOld then confirmPassword aid tm else do
                current <- lift $ runInputPost $ ireq textField "current"
                mrealpass <- lift $ getPassword aid
                case mrealpass of
                    Nothing ->
                        lift $ loginErrorMessage (tm setpassR) "You do not currently have a password set on your account"
                    Just realpass
                        | isValidPass current realpass -> confirmPassword aid tm
                        | otherwise ->
                            lift $ loginErrorMessage (tm setpassR) "Invalid current password, please try again"

  where
    msgOk = Msg.PassUpdated
    confirmPassword aid tm = do
        (new, confirm) <- lift $ runInputPost $ (,)
            <$> ireq textField "new"
            <*> ireq textField "confirm"

        if new /= confirm
          then loginErrorMessageI setpassR Msg.PassMismatch
          else do
              isSecure <- lift $ checkPasswordSecurity aid new
              case isSecure of
                  Left e -> lift $ loginErrorMessage (tm setpassR) e
                  Right () -> do
                      salted <- liftIO $ saltPass new
                      y <- lift $ do
                          setPassword aid salted
                          deleteSession loginLinkKey
                          addMessageI "success" msgOk
                          getYesod

                      mr <- lift getMessageRender
                      selectRep $ do
                        provideRep $
                          fmap asHtml $ lift $ redirect $ afterPasswordRoute y
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
    salt ++ T.unpack (TE.decodeUtf8 $ B16.encode $ H.hash $ TE.encodeUtf8 $ T.pack $ salt ++ pass)

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


defaultNonceGen :: Nonce.Generator
defaultNonceGen = unsafePerformIO (Nonce.new)
{-# NOINLINE defaultNonceGen #-}
