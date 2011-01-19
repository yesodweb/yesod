{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Yesod.Helpers.Auth.Email
    ( authEmail
    , YesodAuthEmail (..)
    , EmailCreds (..)
    , saltPass
    ) where

import Network.Mail.Mime (randomString)
import Yesod.Helpers.Auth
import System.Random
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))
import Data.Digest.Pure.MD5
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

import Yesod.Form
import Yesod.Handler
import Yesod.Content
import Yesod.Widget
import Yesod.Core
import Text.Hamlet (hamlet)
import Control.Monad.IO.Class (liftIO)

login, register, setpass :: AuthRoute
login = PluginR "email" ["login"]
register = PluginR "email" ["register"]
setpass = PluginR "email" ["set-password"]

verify :: String -> String -> AuthRoute -- FIXME
verify eid verkey = PluginR "email" ["verify", eid, verkey]

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

class YesodAuth m => YesodAuthEmail m where
    type AuthEmailId m

    showAuthEmailId :: m -> AuthEmailId m -> String
    readAuthEmailId :: m -> String -> Maybe (AuthEmailId m)

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
    randomKey :: m -> IO String
    randomKey _ = do
        stdgen <- newStdGen
        return $ fst $ randomString 10 stdgen

authEmail :: YesodAuthEmail m => AuthPlugin m
authEmail =
    AuthPlugin "email" dispatch $ \tm -> do
        y <- liftHandler getYesod
#if GHC7
        [hamlet|
#else
        [$hamlet|
#endif
<form method="post" action="@{tm login}">
    <table>
        <tr>
            <th>#{messageEmail y}
            <td>
                <input type="email" name="email">
        <tr>
            <th>#{messagePassword y}
            <td>
                <input type="password" name="password">
        <tr>
            <td colspan="2">
                <input type="submit" value="Login via email">
                <a href="@{tm register}">I don't have an account
|]
  where
    dispatch "GET" ["register"] = getRegisterR >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR >>= sendResponse
    dispatch "GET" ["verify", eid, verkey] = do
        y <- getYesod
        case readAuthEmailId y eid of
            Nothing -> notFound
            Just eid' -> getVerifyR eid' verkey >>= sendResponse
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["set-password"] = getPasswordR >>= sendResponse
    dispatch "POST" ["set-password"] = postPasswordR >>= sendResponse
    dispatch _ _ = notFound

getRegisterR :: YesodAuthEmail master => GHandler Auth master RepHtml
getRegisterR = do
    y <- getYesod
    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle $ messageRegisterLong y
        addHamlet
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
<p>#{messageEnterEmail y}
<form method="post" action="@{toMaster register}">
    <label for="email">#{messageEmail y}
    <input type="email" name="email" width="150">
    <input type="submit" value="#{messageRegister y}">
|]

postRegisterR :: YesodAuthEmail master => GHandler Auth master RepHtml
postRegisterR = do
    y <- getYesod
    email <- runFormPost' $ emailInput "email"
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
    let verUrl = render $ tm $ verify (showAuthEmailId y lid) verKey
    sendVerifyEmail email verKey verUrl
    defaultLayout $ do
        setTitle $ messageConfirmationEmailSentTitle y
        addWidget
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
<p>#{messageConfirmationEmailSent y email}
|]

getVerifyR :: YesodAuthEmail m
           => AuthEmailId m -> String -> GHandler Auth m RepHtml
getVerifyR lid key = do
    realKey <- getVerifyKey lid
    memail <- getEmail lid
    y <- getYesod
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            muid <- verifyAccount lid
            case muid of
                Nothing -> return ()
                Just _uid -> do
                    setCreds False $ Creds "email" email [("verifiedEmail", email)] -- FIXME uid?
                    toMaster <- getRouteToMaster
                    setMessage $ messageAddressVerified y
                    redirect RedirectTemporary $ toMaster setpass
        _ -> return ()
    defaultLayout $ do
        setTitle $ messageInvalidKey y
        addHtml
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
<p>#{messageInvalidKey y}
|]

postLoginR :: YesodAuthEmail master => GHandler Auth master ()
postLoginR = do
    (email, pass) <- runFormPost' $ (,)
        <$> emailInput "email"
        <*> stringInput "password"
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
            y <- getYesod
            setMessage $ messageInvalidEmailPass y
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

getPasswordR :: YesodAuthEmail master => GHandler Auth master RepHtml
getPasswordR = do
    toMaster <- getRouteToMaster
    maid <- maybeAuthId
    y <- getYesod
    case maid of
        Just _ -> return ()
        Nothing -> do
            setMessage $ messageBadSetPass y
            redirect RedirectTemporary $ toMaster login
    defaultLayout $ do
        setTitle $ messageSetPassTitle y
        addHamlet
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
<h3>#{messageSetPass y}
<form method="post" action="@{toMaster setpass}">
    <table>
        <tr>
            <th>#{messageNewPass y}
            <td>
                <input type="password" name="new">
        <tr>
            <th>#{messageConfirmPass y}
            <td>
                <input type="password" name="confirm">
        <tr>
            <td colspan="2">
                <input type="submit" value="#{messageSetPassTitle y}">
|]

postPasswordR :: YesodAuthEmail master => GHandler Auth master ()
postPasswordR = do
    (new, confirm) <- runFormPost' $ (,)
        <$> stringInput "new"
        <*> stringInput "confirm"
    toMaster <- getRouteToMaster
    y <- getYesod
    when (new /= confirm) $ do
        setMessage $ messagePassMismatch y
        redirect RedirectTemporary $ toMaster setpass
    maid <- maybeAuthId
    aid <- case maid of
            Nothing -> do
                setMessage $ messageBadSetPass y
                redirect RedirectTemporary $ toMaster login
            Just aid -> return aid
    salted <- liftIO $ saltPass new
    setPassword aid salted
    setMessage $ messagePassUpdated y
    redirect RedirectTemporary $ loginDest y

saltLength :: Int
saltLength = 5

-- | Salt a password with a randomly generated salt.
saltPass :: String -> IO String
saltPass pass = do
    stdgen <- newStdGen
    let salt = take saltLength $ randomRs ('A', 'Z') stdgen
    return $ saltPass' salt pass

saltPass' :: String -> String -> String
saltPass' salt pass =
    salt ++ show (md5 $ fromString $ salt ++ pass)
  where
    fromString = encodeUtf8 . T.pack

isValidPass :: String -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass clear salted =
    let salt = take saltLength salted
     in salted == saltPass' salt clear
