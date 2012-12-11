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

import Yesod.Form
import Yesod.Handler
import Yesod.Content
import Yesod.Core (PathPiece, fromPathPiece, whamlet, defaultLayout, setTitleI, toPathPiece)
import Control.Monad.IO.Class (liftIO)
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

class (YesodAuth m, PathPiece (AuthEmailId m)) => YesodAuthEmail m where
    type AuthEmailId m

    addUnverified :: Email -> VerKey -> GHandler Auth m (AuthEmailId m)
    sendVerifyEmail :: Email -> VerKey -> VerUrl -> GHandler Auth m ()
    getVerifyKey :: AuthEmailId m -> GHandler Auth m (Maybe VerKey)
    setVerifyKey :: AuthEmailId m -> VerKey -> GHandler Auth m ()
    verifyAccount :: AuthEmailId m -> GHandler Auth m (Maybe (AuthId m))
    getPassword :: AuthId m -> GHandler Auth m (Maybe SaltedPass)
    setPassword :: AuthId m -> SaltedPass -> GHandler Auth m ()
    getEmailCreds :: Email -> GHandler Auth m (Maybe (EmailCreds m))
    getEmail :: AuthEmailId m -> GHandler Auth m (Maybe Email)

    -- | Generate a random alphanumeric string.
    randomKey :: m -> IO Text
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

getRegisterR :: YesodAuthEmail master => GHandler Auth master RepHtml
getRegisterR = do
    toMaster <- getRouteToMaster
    email <- newIdent
    defaultLayout $ do
        setTitleI Msg.RegisterLong
        [whamlet|
$newline never
<p>_{Msg.EnterEmail}
<form method="post" action="@{toMaster registerR}">
    <label for=#{email}>_{Msg.Email}
    <input ##{email} type="email" name="email" width="150">
    <input type="submit" value=_{Msg.Register}>
|]

postRegisterR :: YesodAuthEmail master => GHandler Auth master RepHtml
postRegisterR = do
    y <- getYesod
    email <- runInputPost $ ireq emailField "email"
    mecreds <- getEmailCreds email
    (lid, verKey) <-
        case mecreds of
            Just (EmailCreds lid _ _ (Just key)) -> return (lid, key)
            Just (EmailCreds lid _ _ Nothing) -> do
                key <- liftIO $ randomKey y
                setVerifyKey lid key
                return (lid, key)
            Nothing -> do
                key <- liftIO $ randomKey y
                lid <- addUnverified email key
                return (lid, key)
    render <- getUrlRender
    tm <- getRouteToMaster
    let verUrl = render $ tm $ verify (toPathPiece lid) verKey
    sendVerifyEmail email verKey verUrl
    defaultLayout $ do
        setTitleI Msg.ConfirmationEmailSentTitle
        [whamlet|
$newline never
<p>_{Msg.ConfirmationEmailSent email}
|]

getVerifyR :: YesodAuthEmail m
           => AuthEmailId m -> Text -> GHandler Auth m RepHtml
getVerifyR lid key = do
    realKey <- getVerifyKey lid
    memail <- getEmail lid
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            muid <- verifyAccount lid
            case muid of
                Nothing -> return ()
                Just _uid -> do
                    setCreds False $ Creds "email" email [("verifiedEmail", email)] -- FIXME uid?
                    toMaster <- getRouteToMaster
                    setMessageI Msg.AddressVerified
                    redirect $ toMaster setpassR
        _ -> return ()
    defaultLayout $ do
        setTitleI Msg.InvalidKey
        [whamlet|
$newline never
<p>_{Msg.InvalidKey}
|]

postLoginR :: YesodAuthEmail master => GHandler Auth master ()
postLoginR = do
    (email, pass) <- runInputPost $ (,)
        <$> ireq emailField "email"
        <*> ireq textField "password"
    mecreds <- getEmailCreds email
    maid <-
        case (mecreds >>= emailCredsAuthId, fmap emailCredsStatus mecreds) of
            (Just aid, Just True) -> do
                mrealpass <- getPassword aid
                case mrealpass of
                    Nothing -> return Nothing
                    Just realpass -> return $
                        if isValidPass pass realpass
                            then Just aid
                            else Nothing
            _ -> return Nothing
    case maid of
        Just _aid ->
            setCreds True $ Creds "email" email [("verifiedEmail", email)] -- FIXME aid?
        Nothing -> do
            setMessageI Msg.InvalidEmailPass
            toMaster <- getRouteToMaster
            redirect $ toMaster LoginR

getPasswordR :: YesodAuthEmail master => GHandler Auth master RepHtml
getPasswordR = do
    toMaster <- getRouteToMaster
    maid <- maybeAuthId
    pass1 <- newIdent
    pass2 <- newIdent
    case maid of
        Just _ -> return ()
        Nothing -> do
            setMessageI Msg.BadSetPass
            redirect $ toMaster LoginR
    defaultLayout $ do
        setTitleI Msg.SetPassTitle
        [whamlet|
$newline never
<h3>_{Msg.SetPass}
<form method="post" action="@{toMaster setpassR}">
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
                <input type="submit" value="_{Msg.SetPassTitle}">
|]

postPasswordR :: YesodAuthEmail master => GHandler Auth master ()
postPasswordR = do
    (new, confirm) <- runInputPost $ (,)
        <$> ireq textField "new"
        <*> ireq textField "confirm"
    toMaster <- getRouteToMaster
    y <- getYesod
    when (new /= confirm) $ do
        setMessageI Msg.PassMismatch
        redirect $ toMaster setpassR
    maid <- maybeAuthId
    aid <- case maid of
            Nothing -> do
                setMessageI Msg.BadSetPass
                redirect $ toMaster LoginR
            Just aid -> return aid
    salted <- liftIO $ saltPass new
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
