{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module Yesod.Helpers.Auth2.Email
    ( authEmail
    , YesodAuthEmail (..)
    , EmailCreds (..)
    ) where

import Yesod
import Yesod.Mail (randomString)
import Yesod.Helpers.Auth2
import System.Random
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))
import Data.Digest.Pure.MD5
import Data.String (fromString)
import qualified Data.ByteString.Lazy.UTF8 as LU

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
    AuthPlugin "email" dispatch login'
  where
    go x = x >>= sendResponse
    dispatch "GET" ["register"] = go getRegisterR
    dispatch "POST" ["register"] = go postRegisterR
    dispatch "GET" ["verify", eid, verkey] = do
        y <- getYesod
        case readAuthEmailId y eid of
            Nothing -> notFound
            Just eid' -> go $ getVerifyR eid' verkey
    dispatch "POST" ["login"] = go postLoginR
    dispatch "GET" ["set-password"] = go getPasswordR
    dispatch "POST" ["set-password"] = go postPasswordR
    dispatch _ _ = notFound

    login' = do
        tm <- liftHandler getRouteToMaster
        addBody [$hamlet|
%form!method=post!action=@tm.login@
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
                %a!href=@tm.register@ I don't have an account
|]

getRegisterR :: YesodAuthEmail master => GHandler Auth master RepHtml
getRegisterR = do
    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle $ string "Register a new account"
        addBody [$hamlet|
%p Enter your e-mail address below, and a confirmation e-mail will be sent to you.
%form!method=post!action=@toMaster.register@
    %label!for=email E-mail
    %input#email!type=email!name=email!width=150
    %input!type=submit!value=Register
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
        setTitle $ string "Confirmation e-mail sent"
        addBody [$hamlet|
%p A confirmation e-mail has been sent to $email$.
|]

getVerifyR :: YesodAuthEmail m
           => AuthEmailId m -> String -> GHandler Auth m RepHtml
getVerifyR lid key = do
    realKey <- getVerifyKey lid
    memail <- getEmail lid
    case (realKey == Just key, memail) of
        (True, Just email) -> do
            muid <- verifyAccount lid
            case muid of
                Nothing -> return ()
                Just uid -> do
                    setCreds False $ Creds "email" email [("verifiedEmail", email)] -- FIXME uid?
                    toMaster <- getRouteToMaster
                    setMessage $ string "Address verified, please set a new password"
                    redirect RedirectTemporary $ toMaster setpass
        _ -> return ()
    defaultLayout $ do
        setTitle $ string "Invalid verification key"
        addBody [$hamlet|
%p I'm sorry, but that was an invalid verification key.
|]

postLoginR :: YesodAuthEmail master => GHandler Auth master ()
postLoginR = do
    (email, pass) <- runFormPost' $ (,)
        <$> emailInput "email"
        <*> stringInput "password"
    y <- getYesod
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
        Just aid ->
            setCreds True $ Creds "email" email [("verifiedEmail", email)] -- FIXME aid?
        Nothing -> do
            setMessage $ string "Invalid email/password combination"
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

getPasswordR :: YesodAuthEmail master => GHandler Auth master RepHtml
getPasswordR = do
    toMaster <- getRouteToMaster
    maid <- maybeAuthId
    case maid of
        Just _ -> return ()
        Nothing -> do
            setMessage $ string "You must be logged in to set a password"
            redirect RedirectTemporary $ toMaster login
    defaultLayout $ do
        setTitle $ string "Set password"
        addBody [$hamlet|
%h3 Set a new password
%form!method=post!action=@toMaster.setpass@
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

postPasswordR :: YesodAuthEmail master => GHandler Auth master ()
postPasswordR = do
    (new, confirm) <- runFormPost' $ (,)
        <$> stringInput "new"
        <*> stringInput "confirm"
    toMaster <- getRouteToMaster
    when (new /= confirm) $ do
        setMessage $ string "Passwords did not match, please try again"
        redirect RedirectTemporary $ toMaster setpass
    maid <- maybeAuthId
    aid <- case maid of
            Nothing -> do
                setMessage $ string "You must be logged in to set a password"
                redirect RedirectTemporary $ toMaster login
            Just aid -> return aid
    salted <- liftIO $ saltPass new
    setPassword aid salted
    setMessage $ string "Password updated"
    y <- getYesod
    redirect RedirectTemporary $ defaultDest y

saltLength :: Int
saltLength = 5

saltPass :: String -> IO String
saltPass pass = do
    stdgen <- newStdGen
    let salt = take saltLength $ randomRs ('A', 'Z') stdgen
    return $ saltPass' salt pass

saltPass' :: String -> String -> String
saltPass' salt pass = salt ++ show (md5 $ LU.fromString $ salt ++ pass)

isValidPass :: String -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass clear salted =
    let salt = take saltLength salted
     in salted == saltPass' salt clear
