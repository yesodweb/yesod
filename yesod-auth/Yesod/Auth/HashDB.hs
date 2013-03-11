{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE CPP                        #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Auth.HashDB
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  Stable
-- Portability :  Portable
--
-- A yesod-auth AuthPlugin designed to look users up in Persist where
-- their user id's and a salted SHA1 hash of their password is stored.
--
-- Example usage:
--
-- > -- import the function
-- > import Auth.HashDB
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
-- >    getAuthId    = getAuthIdHashDB AuthR (Just . UniqueUser)
-- >    authPlugins  = [authHashDB (Just . UniqueUser)]
-- >
-- >
-- > -- include the migration function in site startup
-- > withServer :: (Application -> IO a) -> IO a
-- > withServer f = withConnectionPool $ \p -> do
-- >     runSqlPool (runMigration migrateUsers) p
-- >     let h = DevSite p
--
-- Note that function which converts username to unique identifier must be same.
--
-- Your app must be an instance of YesodPersist. and the username,
-- salt and hashed-passwords should be added to the database.
--
-- > echo -n 'MySaltMyPassword' | sha1sum
--
-- can be used to get the hash from the commandline.
--
-------------------------------------------------------------------------------
module Yesod.Auth.HashDB
    ( HashDBUser(..)
    , Unique (..)
    , setPassword
      -- * Authentification
    , validateUser
    , authHashDB
    , getAuthIdHashDB
      -- * Predefined data type
    , User
    , UserGeneric (..)
    , UserId
    , migrateUsers
    ) where

import Yesod.Persist
import Yesod.Handler
import Yesod.Form
import Yesod.Auth
import Yesod.Widget (toWidget)
import Text.Hamlet (hamlet)

import Control.Applicative         ((<$>), (<*>))
import Control.Monad               (replicateM,liftM)
import Control.Monad.IO.Class      (MonadIO, liftIO)

import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import Data.Digest.Pure.SHA        (sha1, showDigest)
import Data.Text                   (Text, pack, unpack, append)
import Data.Maybe                  (fromMaybe)
import System.Random               (randomRIO)

-- | Interface for data type which holds user info. It's just a
--   collection of getters and setters
class HashDBUser user where
  -- | Retrieve password hash from user data
  userPasswordHash :: user -> Maybe Text
  -- | Retrieve salt for password
  userPasswordSalt :: user -> Maybe Text

  -- | Deprecated for the better named setSaltAndPasswordHash 
  setUserHashAndSalt :: Text    -- ^ Salt
                     -> Text    -- ^ Password hash
                     -> user -> user
  setUserHashAndSalt = setSaltAndPasswordHash

  -- | a callback for setPassword
  setSaltAndPasswordHash :: Text    -- ^ Salt
                     -> Text    -- ^ Password hash
                     -> user -> user
  setSaltAndPasswordHash = setUserHashAndSalt

-- | Generate random salt. Length of 8 is chosen arbitrarily
randomSalt :: MonadIO m => m Text
randomSalt = pack `liftM` liftIO (replicateM 8 (randomRIO ('0','z')))

-- | Calculate salted hash using SHA1.
saltedHash :: Text              -- ^ Salt
           -> Text              -- ^ Password
           -> Text
saltedHash salt = 
  pack . showDigest . sha1 . BS.pack . unpack . append salt

-- | Set password for user. This function should be used for setting
--   passwords. It generates random salt and calculates proper hashes.
setPassword :: (MonadIO m, HashDBUser user) => Text -> user -> m user
setPassword pwd u = do salt <- randomSalt
                       return $ setSaltAndPasswordHash salt (saltedHash salt pwd) u


----------------------------------------------------------------
-- Authentification
----------------------------------------------------------------

-- | Given a user ID and password in plaintext, validate them against
--   the database values.
validateUser :: ( YesodPersist yesod
                , b ~ YesodPersistBackend yesod
                , PersistMonadBackend (b (GHandler sub yesod)) ~ PersistEntityBackend user
                , PersistUnique (b (GHandler sub yesod))
                , PersistEntity user
                , HashDBUser    user
                ) => 
                Unique user     -- ^ User unique identifier
             -> Text            -- ^ Password in plaint-text
             -> GHandler sub yesod Bool
validateUser userID passwd = do
  -- Checks that hash and password match
  let validate u = do hash <- userPasswordHash u
                      salt <- userPasswordSalt u
                      return $ hash == saltedHash salt passwd
  -- Get user data
  user <- runDB $ getBy userID
  return $ fromMaybe False $ validate . entityVal =<< user


login :: AuthRoute
login = PluginR "hashdb" ["login"]


-- | Handle the login form. First parameter is function which maps
--   username (whatever it might be) to unique user ID.
postLoginR :: ( YesodAuth y, YesodPersist y
              , HashDBUser user, PersistEntity user
              , b ~ YesodPersistBackend y
              , PersistMonadBackend (b (GHandler Auth y)) ~ PersistEntityBackend user
              , PersistUnique (b (GHandler Auth y))
              )
           => (Text -> Maybe (Unique user))
           -> GHandler Auth y ()
postLoginR uniq = do
    (mu,mp) <- runInputPost $ (,)
        <$> iopt textField "username"
        <*> iopt textField "password"

    isValid <- fromMaybe (return False) 
                 (validateUser <$> (uniq =<< mu) <*> mp)
    if isValid 
       then setCreds True $ Creds "hashdb" (fromMaybe "" mu) []
       else do setMessage "Invalid username/password"
               toMaster <- getRouteToMaster
               redirect $ toMaster LoginR


-- | A drop in for the getAuthId method of your YesodAuth instance which
--   can be used if authHashDB is the only plugin in use.
getAuthIdHashDB :: ( YesodAuth master, YesodPersist master
                   , HashDBUser user, PersistEntity user
                   , Key user ~ AuthId master
                   , b ~ YesodPersistBackend master
                   , PersistMonadBackend (b (GHandler sub master)) ~ PersistEntityBackend user
                   , PersistUnique (b (GHandler sub master))
                   )
                => (AuthRoute -> Route master)   -- ^ your site's Auth Route
                -> (Text -> Maybe (Unique user)) -- ^ gets user ID
                -> Creds master                  -- ^ the creds argument
                -> GHandler sub master (Maybe (AuthId master))
getAuthIdHashDB authR uniq creds = do
    muid <- maybeAuthId
    case muid of
        -- user already authenticated
        Just uid -> return $ Just uid
        Nothing       -> do
            x <- case uniq (credsIdent creds) of
                   Nothing -> return Nothing
                   Just u  -> runDB (getBy u)
            case x of
                -- user exists
                Just (Entity uid _) -> return $ Just uid
                Nothing       -> do
                    setMessage "User not found"
                    redirect $ authR LoginR

-- | Prompt for username and password, validate that against a database
--   which holds the username and a hash of the password
authHashDB :: ( YesodAuth m, YesodPersist m
              , HashDBUser user
              , PersistEntity user
              , b ~ YesodPersistBackend m
              , PersistMonadBackend (b (GHandler Auth m)) ~ PersistEntityBackend user
              , PersistUnique (b (GHandler Auth m)))
           => (Text -> Maybe (Unique user)) -> AuthPlugin m
authHashDB uniq = AuthPlugin "hashdb" dispatch $ \tm -> toWidget [hamlet|
$newline never
    <div id="header">
        <h1>Login

    <div id="login">
        <form method="post" action="@{tm login}">
            <table>
                <tr>
                    <th>Username:
                    <td>
                        <input id="x" name="username" autofocus="" required>
                <tr>
                    <th>Password:
                    <td>
                        <input type="password" name="password" required>
                <tr>
                    <td>&nbsp;
                    <td>
                        <input type="submit" value="Login">

            <script>
                if (!("autofocus" in document.createElement("input"))) {
                    document.getElementById("x").focus();
                }

|]
    where
        dispatch "POST" ["login"] = postLoginR uniq >>= sendResponse
        dispatch _ _              = notFound


----------------------------------------------------------------
-- Predefined datatype
----------------------------------------------------------------

-- | Generate data base instances for a valid user
share [mkPersist sqlSettings, mkMigrate "migrateUsers"]
         [persistUpperCase|
User
    username Text Eq
    password Text
    salt     Text
    UniqueUser username
|]

instance HashDBUser (UserGeneric backend) where
  userPasswordHash = Just . userPassword
  userPasswordSalt = Just . userSalt
  setSaltAndPasswordHash s h u = u { userSalt     = s
                               , userPassword = h
                               }
