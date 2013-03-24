{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Auth.Email
    ( -- * Plugin
      authEmail
    , YesodAuthEmail (..)
    , EmailCreds (..)
    , saltPass
      -- * Routes
    , loginR
    , registerR
    , setpassR
    , isValidPass
    ) where

import Network.Mail.Mime (randomString)
import Yesod.Auth
import System.Random
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))
import Data.Digest.Pure.MD5
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text (Text)
import qualified Crypto.PasswordStore as PS
import qualified Data.Text.Encoding as DTE
import Control.Monad.Trans.Class

import Yesod.Form
import Yesod.Core
import qualified Yesod.Auth.Message as Msg

loginR, registerR, setpassR :: AuthRoute
loginR = PluginR "email" ["login"]
registerR = PluginR "email" ["register"]
setpassR = PluginR "email" ["set-password"]

verify :: Text -> Text -> AuthRoute -- FIXME
verify eid verkey = PluginR "email" ["verify", eid, verkey]

type Email = Text
type VerKey = Text
type VerUrl = Text
type SaltedPass = Text
type VerStatus = Bool

-- | Data stored in a database for each e-mail address.
data EmailCreds m = EmailCreds
    { emailCredsId :: AuthEmailId m
    , emailCredsAuthId :: Maybe (AuthId m)
    , emailCredsStatus :: VerStatus
    , emailCredsVerkey :: Maybe VerKey
    }

class (YesodAuth site, PathPiece (AuthEmailId site)) => YesodAuthEmail site where
    type AuthEmailId site

    addUnverified :: Email -> VerKey -> HandlerT site IO (AuthEmailId site)
    sendVerifyEmail :: Email -> VerKey -> VerUrl -> HandlerT site IO ()
    getVerifyKey :: AuthEmailId site -> HandlerT site IO (Maybe VerKey)
    setVerifyKey :: AuthEmailId site -> VerKey -> HandlerT site IO ()
    verifyAccount :: AuthEmailId site -> HandlerT site IO (Maybe (AuthId site))
    getPassword :: AuthId site -> HandlerT site IO (Maybe SaltedPass)
    setPassword :: AuthId site -> SaltedPass -> HandlerT site IO ()
    getEmailCreds :: Email -> HandlerT site IO (Maybe (EmailCreds site))
    getEmail :: AuthEmailId site -> HandlerT site IO (Maybe Email)

    -- | Generate a random alphanumeric string.
    randomKey :: site -> IO Text
    randomKey _ = do
        stdgen <- newStdGen
        return $ TS.pack $ fst $ randomString 10 stdgen

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
    dispatch "GET" ["verify", eid, verkey] =
        case fromPathPiece eid of
            Nothing -> notFound
            Just eid' -> getVerifyR eid' verkey >>= sendResponse
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["set-password"] = getPasswordR >>= sendResponse
    dispatch "POST" ["set-password"] = postPasswordR >>= sendResponse
    dispatch _ _ = notFound

getRegisterR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) RepHtml
getRegisterR = do
    email <- newIdent
    tp <- getRouteToParent
    lift $ defaultLayout $ do
        setTitleI Msg.RegisterLong
        [whamlet|
            <p>_{Msg.EnterEmail}
            <form method="post" action="@{tp registerR}">
                <label for=#{email}>_{Msg.Email}
                <input ##{email} type="email" name="email" width="150">
                <input type="submit" value=_{Msg.Register}>
        |]

postRegisterR :: YesodAuthEmail master => AuthHandler master RepHtml
postRegisterR = do
    y <- lift getYesod
    email <- lift $ runInputPost $ ireq emailField "email"
    mecreds <- lift $ getEmailCreds email
    (lid, verKey) <-
        case mecreds of
            Just (EmailCreds lid _ _ (Just key)) -> return (lid, key)
            Just (EmailCreds lid _ _ Nothing) -> do
                key <- liftIO $ randomKey y
                lift $ setVerifyKey lid key
                return (lid, key)
            Nothing -> do
                key <- liftIO $ randomKey y
                lid <- lift $ addUnverified email key
                return (lid, key)
    render <- getUrlRender
    let verUrl = render $ verify (toPathPiece lid) verKey
    lift $ sendVerifyEmail email verKey verUrl
    lift $ defaultLayout $ do
        setTitleI Msg.ConfirmationEmailSentTitle
        [whamlet|<p>_{Msg.ConfirmationEmailSent email}|]

getVerifyR :: YesodAuthEmail m
           => AuthEmailId m
           -> Text
           -> HandlerT Auth (HandlerT m IO) RepHtml
getVerifyR lid key = do
    realKey <- lift $ getVerifyKey lid
    memail <- lift $ getEmail lid
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            muid <- lift $ verifyAccount lid
            case muid of
                Nothing -> return ()
                Just _uid -> do
                    lift $ setCreds False $ Creds "email" email [("verifiedEmail", email)] -- FIXME uid?
                    lift $ setMessageI Msg.AddressVerified
                    redirect setpassR
        _ -> return ()
    lift $ defaultLayout $ do
        setTitleI Msg.InvalidKey
        [whamlet|<p>_{Msg.InvalidKey}|]

postLoginR :: YesodAuthEmail master => AuthHandler master ()
postLoginR = do
    (email, pass) <- lift $ runInputPost $ (,)
        <$> ireq emailField "email"
        <*> ireq textField "password"
    mecreds <- lift $ getEmailCreds email
    maid <-
        case (mecreds >>= emailCredsAuthId, fmap emailCredsStatus mecreds) of
            (Just aid, Just True) -> do
                mrealpass <- lift $ getPassword aid
                case mrealpass of
                    Nothing -> return Nothing
                    Just realpass -> return $
                        if isValidPass pass realpass
                            then Just aid
                            else Nothing
            _ -> return Nothing
    case maid of
        Just _aid ->
            lift $ setCreds True $ Creds "email" email [("verifiedEmail", email)] -- FIXME aid?
        Nothing -> do
            lift $ setMessageI Msg.InvalidEmailPass
            redirect LoginR

getPasswordR :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) RepHtml
getPasswordR = do
    maid <- lift maybeAuthId
    pass1 <- newIdent
    pass2 <- newIdent
    case maid of
        Just _ -> return ()
        Nothing -> do
            lift $ setMessageI Msg.BadSetPass
            redirect LoginR
    tp <- getRouteToParent
    lift $ defaultLayout $ do
        setTitleI Msg.SetPassTitle
        [whamlet|
$newline never
<h3>_{Msg.SetPass}
<form method="post" action="@{tp setpassR}">
    <table>
        <tr>
            <th>
                <label for=#{pass1}>_{Msg.NewPass}
            <td>
                <input ##{pass1} type="password" name="new">
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
    (new, confirm) <- lift $ runInputPost $ (,)
        <$> ireq textField "new"
        <*> ireq textField "confirm"
    when (new /= confirm) $ do
        lift $ setMessageI Msg.PassMismatch
        redirect setpassR
    maid <- lift maybeAuthId
    aid <- case maid of
            Nothing -> do
                lift $ setMessageI Msg.BadSetPass
                redirect LoginR
            Just aid -> return aid
    salted <- liftIO $ saltPass new
    lift $ do
        y <- getYesod
        setPassword aid salted
        setMessageI Msg.PassUpdated
        redirect $ loginDest y

saltLength :: Int
saltLength = 5

-- | Salt a password with a randomly generated salt.
saltPass :: Text -> IO Text
saltPass = fmap DTE.decodeUtf8
         . flip PS.makePassword 12
         . DTE.encodeUtf8

saltPass' :: String -> String -> String
saltPass' salt pass =
    salt ++ show (md5 $ fromString $ salt ++ pass)
  where
    fromString = encodeUtf8 . T.pack

isValidPass :: Text -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass ct salted =
    PS.verifyPassword (DTE.encodeUtf8 ct) (DTE.encodeUtf8 salted) || isValidPass' ct salted

isValidPass' :: Text -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass' clear' salted' =
    let salt = take saltLength salted
     in salted == saltPass' salt clear
  where
    clear = TS.unpack clear'
    salted = TS.unpack salted'
