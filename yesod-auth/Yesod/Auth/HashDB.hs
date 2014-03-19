{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ConstraintKinds            #-}
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
-- their user id's and a Bcrypt hash + salt of their password is stored.
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
-- salted-and-hashed-passwords should be added to the database.
--
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
    , Siteuser
    , SiteuserGeneric (..)
    , SiteuserId
    , EntityField (..)
    , migrateSiteusers
    ) where

import Yesod.Persist
import Yesod.Form
import Yesod.Auth
import Yesod.Core
import Text.Hamlet (hamlet)

import Control.Applicative         ((<$>), (<*>))
import Control.Monad               (replicateM,liftM)
import Data.Typeable

import qualified Data.ByteString.Lazy.Char8 as LBS (pack, unpack)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Crypto.BCrypt
import Data.Text                   (Text, pack, unpack, append)
import Data.Maybe                  
import System.Random               (randomRIO)
import Prelude
-- | Interface for data type which holds user info. It's just a
--   collection of getters and setters
class HashDBUser siteuser where
  -- | Retrieve password hash from user data
  siteuserPasswordHash :: siteuser -> Maybe Text


  -- | a callback for setPassword
  setSaltAndPasswordHash :: Text    -- ^ Hash and Salt
                     -> siteuser -> siteuser

-- | Calculate salted hash using Bcrypt.
saltedHash :: Text              -- ^ Password
           -> IO (Maybe Text)
saltedHash password = do
   hash <- (hashPasswordUsingPolicy (HashingPolicy 10 "$2y") . BS.pack . unpack) password
   return $ if (isJust hash)
                then Just $ pack $ BS.unpack $ fromJust hash
                else Nothing

-- | Set password for user. This function should be used for setting
--   passwords. It generates random salt and calculates proper hashes.
setPassword :: (HashDBUser siteuser) => Text -> siteuser -> IO (siteuser)
setPassword pwd u = do 
    hash <- saltedHash pwd
    return $ setSaltAndPasswordHash (fromJust hash) u


----------------------------------------------------------------
-- Authentification
----------------------------------------------------------------

-- | Given a user ID and password in plaintext, validate them against
--   the database values.
validateUser :: ( YesodPersist yesod
                , b ~ YesodPersistBackend yesod
                , PersistMonadBackend (b (HandlerT yesod IO)) ~ PersistEntityBackend siteuser
                , PersistUnique (b (HandlerT yesod IO))
                , PersistEntity siteuser
                , HashDBUser    siteuser
                ) => 
                Unique siteuser     -- ^ User unique identifier
             -> Text            -- ^ Password in plaint-text
             -> HandlerT yesod IO Bool
validateUser siteuserID passwd = do
  -- Checks that hash and password match
  let validate u = do hash <- siteuserPasswordHash u
                      return $ validatePassword (BS.pack $ unpack hash) (BS.pack $ unpack passwd)
  -- Get user data
  siteuser <- runDB $ getBy siteuserID
  return $ fromMaybe False $ validate . entityVal =<< siteuser


login :: AuthRoute
login = PluginR "hashdb" ["login"]


-- | Handle the login form. First parameter is function which maps
--   username (whatever it might be) to unique user ID.
postLoginR :: ( YesodAuth y, YesodPersist y
              , HashDBUser siteuser, PersistEntity siteuser
              , b ~ YesodPersistBackend y
              , PersistMonadBackend (b (HandlerT y IO)) ~ PersistEntityBackend siteuser
              , PersistUnique (b (HandlerT y IO))
              )
           => (Text -> Maybe (Unique siteuser))
           -> HandlerT Auth (HandlerT y IO) ()
postLoginR uniq = do
    (mu,mp) <- lift $ runInputPost $ (,)
        <$> iopt textField "username"
        <*> iopt textField "password"

    isValid <- lift $ fromMaybe (return False) 
                 (validateUser <$> (uniq =<< mu) <*> mp)
    if isValid 
       then lift $ setCreds True $ Creds "hashdb" (fromMaybe "" mu) []
       else do
           tm <- getRouteToParent
           lift $ loginErrorMessage (tm LoginR) "Invalid username/password"


-- | A drop in for the getAuthId method of your YesodAuth instance which
--   can be used if authHashDB is the only plugin in use.
getAuthIdHashDB :: ( YesodAuth master, YesodPersist master
                   , HashDBUser siteuser, PersistEntity siteuser
                   , Key siteuser ~ AuthId master
                   , b ~ YesodPersistBackend master
                   , PersistMonadBackend (b (HandlerT master IO)) ~ PersistEntityBackend siteuser
                   , PersistUnique (b (HandlerT master IO))
                   )
                => (AuthRoute -> Route master)   -- ^ your site's Auth Route
                -> (Text -> Maybe (Unique siteuser)) -- ^ gets user ID
                -> Creds master                  -- ^ the creds argument
                -> HandlerT master IO (Maybe (AuthId master))
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
                Nothing       -> loginErrorMessage (authR LoginR) "User not found"

-- | Prompt for username and password, validate that against a database
--   which holds the username and a hash of the password
authHashDB :: ( YesodAuth m, YesodPersist m
              , HashDBUser siteuser
              , PersistEntity siteuser
              , b ~ YesodPersistBackend m
              , PersistMonadBackend (b (HandlerT m IO)) ~ PersistEntityBackend siteuser
              , PersistUnique (b (HandlerT m IO)))
           => (Text -> Maybe (Unique siteuser)) -> AuthPlugin m
authHashDB uniq = AuthPlugin "hashdb" dispatch $ \tm -> toWidget [hamlet|
$newline never
    <div id="header">
        <h1>Login

    <div id="login">
        <form method="post" action="@{tm login}">
            <fieldset>
                <label for="username">Username
                <input type="text" name="username">
                <label for="password">Password
                <input type="password" name="password">
                <br />
                <input type="submit" value="Login">


|]
    where
        dispatch "POST" ["login"] = postLoginR uniq >>= sendResponse
        dispatch _ _              = notFound


----------------------------------------------------------------
-- Predefined datatype
----------------------------------------------------------------

-- | Generate data base instances for a valid user
share [mkPersist sqlSettings, mkMigrate "migrateSiteusers"]
         [persistLowerCase|
Siteuser
    username Text Eq
    password Text
    email Text Maybe
    UniqueSiteuser username
    deriving Typeable
|]

instance HashDBUser (SiteuserGeneric backend) where
  siteuserPasswordHash = Just . siteuserPassword
  setSaltAndPasswordHash h u = u { siteuserPassword = h }
