{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Helpers.Auth.HashDB
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  Stable
-- Portability :  Portable
--
-- A yesod-auth AuthPlugin designed to look users up in Persist where
-- their user id's and a sha1 hash of their password will already be
-- stored.
--
-- Example usage:
--
-- > -- import the function
-- > import Helpers.Auth.HashDB
-- >
-- > -- make sure you have an auth route
-- > mkYesodData "MyApp" [$parseRoutes|
-- > / RootR GET
-- > /auth AuthR Auth getAuth
-- > |]
-- >
-- >
-- > -- make your app an instance of YesodAuth using this plugin
-- > instance YesodAuth MyApp where
-- >    type AuthId MyApp = UserId
-- >
-- >    loginDest _  = RootR
-- >    logoutDest _ = RootR
-- >    getAuthId    = getAuthIdHashDB AuthR 
-- >    showAuthId _ = showIntegral
-- >    readAuthId _ = readIntegral
-- >    authPlugins  = [authHashDB]
-- >
-- >
-- > -- include the migration function in site startup
-- > withServer :: (Application -> IO a) -> IO a
-- > withServer f = withConnectionPool $ \p -> do
-- >     runSqlPool (runMigration migrateUsers) p
-- >     let h = DevSite p
--
-- Your app must be an instance of YesodPersist and the username and
-- hashed-passwords must be added manually to the database.
--
-- > echo -n 'MyPassword' | sha1sum
--
-- can be used to get the hash from the commandline.
--
-------------------------------------------------------------------------------
module Yesod.Helpers.Auth.HashDB
    ( authHashDB
    , getAuthIdHashDB
    , UserId
    , migrateUsers
    ) where

import Yesod.Persist
import Yesod.Handler
import Yesod.Form
import Yesod.Helpers.Auth
import Text.Hamlet (hamlet)

import Control.Applicative         ((<$>), (<*>))
import Data.ByteString.Lazy.Char8  (pack)
import Data.Digest.Pure.SHA        (sha1, showDigest)
import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

-- | Computer the sha1 of a string and return it as a string
sha1String :: String -> String
sha1String = showDigest . sha1 . pack

-- | Generate data base instances for a valid user
share2 mkPersist (mkMigrate "migrateUsers") [$persist|
User
    username String Eq
    password String
    UniqueUser username
|]

-- | Given a (user,password) in plaintext, validate them against the
--   database values
validateUser :: (YesodPersist y, 
                 PersistBackend (YesodDB y (GGHandler sub y IO))) 
             => (String, String) 
             -> GHandler sub y Bool
validateUser (user,password) = runDB (getBy $ UniqueUser user) >>= \dbUser ->
    case dbUser of
        -- user not found
        Nothing          -> return False
        -- validate password
        Just (_, sqlUser) -> return $ sha1String password == userPassword sqlUser

login :: AuthRoute
login = PluginR "hashdb" ["login"]

-- | Handle the login form
postLoginR :: (YesodAuth y,
               YesodPersist y, 
               PersistBackend (YesodDB y (GGHandler Auth y IO)))
           => GHandler Auth y ()
postLoginR = do
    (user, password) <- runFormPost' $ (,)
        <$> stringInput "username"
        <*> stringInput "password"

    isValid <- validateUser (user,password)

    if isValid
        then setCreds True $ Creds "hashdb" user []
        else do
            setMessage $ [$hamlet| <em>invalid username/password 
|]
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

-- | A drop in for the getAuthId method of your YesodAuth instance which
--   can be used if authHashDB is the only plugin in use.
getAuthIdHashDB :: (Key User ~ AuthId master,
                    PersistBackend (YesodDB master (GGHandler sub master IO)),
                    YesodPersist master,
                    YesodAuth master)
                => (AuthRoute -> Route master) -- ^ your site's Auth Route
                -> Creds m                     -- ^ the creds argument
                -> GHandler sub master (Maybe UserId)
getAuthIdHashDB authR creds = do
    muid <- maybeAuth
    case muid of
        -- user already authenticated
        Just (uid, _) -> return $ Just uid
        Nothing       -> do
            x <- runDB $ getBy $ UniqueUser (credsIdent creds)
            case x of
                -- user exists
                Just (uid, _) -> return $ Just uid
                Nothing       -> do
                    setMessage $ [$hamlet| <em>user not found 
|]
                    redirect RedirectTemporary $ authR LoginR

-- | Prompt for username and password, validate that against a database
--   which holds the username and a hash of the password
authHashDB :: (YesodAuth y,
               YesodPersist y, 
               PersistBackend (YesodDB y (GGHandler Auth y IO)))
           => AuthPlugin y
authHashDB = AuthPlugin "hashdb" dispatch $ \tm ->
    [$hamlet|\
    <div id="header">
        <h1>Login
\
    <div id="login">
        <form method="post" action="@{tm login}">
            <table>
                <tr>
                    <th>Username:
                    <td>
                        <input id="x" name="username" autofocus="">
                <tr>
                    <th>Password:
                    <td>
                        <input type="password" name="password">
                <tr>
                    <td>&nbsp;
                    <td>
                        <input type="submit" value="Login">
\
            <script>
                \if (!("autofocus" in document.createElement("input"))) {
                    \document.getElementById("x").focus();
                \}
    \
|]
    where
        dispatch "POST" ["login"] = postLoginR >>= sendResponse
        dispatch _ _              = notFound
