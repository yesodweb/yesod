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
-- Maintainer  :  Paul Rouse <pyr@doynton.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- A yesod-auth AuthPlugin designed to look users up in Persist where
-- their user id's and a hash of their password is stored.
--
-- This module was removed at @yesod-auth-1.3.0.0@ and reintroduced 
-- into @yesod-auth-1.3.1.0@, so your dependency on @yesod-auth@ should 
-- take that into account.  For example:
--
-- > yesod-auth >= 1.3.1 && < 1.4
--
-- Versions of this module prior to yesod-auth-1.3 used a relatively weak
-- hashing algorithm (a single round of SHA1) which does not provide
-- adequate protection against an attacker who discovers the hashed passwords.
-- See: <https://github.com/yesodweb/yesod/issues/668>.
--
-- It has now been rewritten to use "Crypto.PasswordStore", but this has been
-- done in a way which preserves compatibility both with the API and
-- with databases which have been set up using older versions of this module.
-- There are two levels of database compatibility:
--
--     * The verification code recognises both the old and new hash formats,
--       so passwords can be verified against database entries which still
--       contain old-style hashes.
--
--     * The function 'upgradePasswordHash' can be used to migrate
--       existing user records to use the new format hash.  Unlike
--       freshly created password hashes, entries converted this way
--       must still have the old salt field, since the old hash function
--       remains part of the algorithm needed for verification.  (The
--       new hash is layered on top of the old one.)
--
-- On the other hand, new passwords set up by 'setPassword' or
-- 'setPasswordStrength' no longer use a separate salt field, so new users
-- of this module need only provide a single password field in the user data,
-- and can ignore the salt.
--
-- In a system which has been migrated from the old format, passwords
-- which are reset using the new format will have an empty salt field.
-- Once all the entries are of this form, it is safe to change the model
-- to remove the salt, and change the 'HashDBUser' instance accordingly.
--
-- To use this in a Yesod application, it must be an instance of
-- YesodPersist, and the username and hashed-passwords should be added
-- to the database.  The followng steps give an outline of what is required.
--
-- You need a database table to store user records: in a scaffolded site it
-- might look like:
--
-- > User
-- >     name Text             -- user name used by HashDB
-- >     password Text Maybe   -- password hash for HashDB
-- >     UniqueUser name
--
-- Create an instance of 'HashDBUser' for this data type:
--
-- > instance HashDBUser User where
-- >     userPasswordHash = userPassword
-- >     setPasswordHash h p = p { userPassword = Just h }
--
-- In the YesodAuth instance declaration for your app, include 'authHashDB'
-- like so:
--
-- > instance YesodAuth App where
-- >     ....
-- >     authPlugins _ = [ authHashDB (Just . UniqueUser), .... ]
-- >     getAuthId = getAuthIdHashDB AuthR (Just . UniqueUser)
--
-- @AuthR@ should be your authentication route, and the function
-- @(Just . UniqueUser)@ supplied to both 'authHashDB' and
-- 'getAuthIdHashDB' takes a 'Text' and produces a 'Unique' value to
-- look up in the User table.  'getAuthIdHashDB' is just a convenience
-- for the case when 'HashDB' is the only plugin, and something else
-- would be needed when other plugins are used as well.
--
-- You can create password hashes manually as follows, if you need to
-- initialise the database:
--
-- > ghci -XOverloadedStrings
-- > > import Crypto.PasswordStore
-- > > makePassword "MyPassword" 14
--
-- where \"14\" is the default strength parameter used in this module.
--
-------------------------------------------------------------------------------
module Yesod.Auth.HashDB
    ( HashDBUser(..)
    , Unique (..)
    , defaultStrength
    , setPasswordStrength
    , setPassword
    , upgradePasswordHash
      -- * Authentification
    , validateUser
    , authHashDB
    , getAuthIdHashDB
      -- * Predefined data type
    , User
    , UserGeneric (..)
    , UserId
    , EntityField (..)
    , migrateUsers
    ) where

import Yesod.Persist
import Yesod.Form
import Yesod.Auth
import Yesod.Core

import Control.Applicative         ((<$>), (<*>))
import Data.Typeable

import qualified Data.ByteString.Lazy.Char8 as BL (pack)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.Digest.Pure.SHA        (sha1, showDigest)
import Data.Text                   (Text, pack, unpack, append)
import Data.Maybe                  (fromMaybe)
import Crypto.PasswordStore        (makePassword, verifyPassword,
                                    passwordStrength, strengthenPassword)

-- | Default strength used for passwords (see "Crypto.PasswordStore" for
--   details).
defaultStrength :: Int
defaultStrength = 14

-- | Interface for data type which holds user info. It's just a
--   collection of getters and setters
class HashDBUser user where
  -- | Retrieve password hash from user data
  userPasswordHash :: user -> Maybe Text
  -- | Retrieve salt for password from user data.  This is needed only for
  --   compatibility with old database entries, which contain the salt
  --   as a separate field.  New implementations do not require a separate
  --   salt field in the user data, and should leave this as the default.
  userPasswordSalt :: user -> Maybe Text
  userPasswordSalt _ = Just ""

  -- | Callback for 'setPassword' and 'upgradePasswordHash'.  Produces a
  --   version of the user data with the hash set to the new value.
  --
  --   This is the method which you should define for new applications, which
  --   do not require compatibility with databases containing hashes written
  --   by previous versions of this module.  If you do need compatibility,
  --   define 'setSaltAndPasswordHash' instead.
  setPasswordHash :: Text   -- ^ Password hash
                     -> user -> user
  setPasswordHash = setSaltAndPasswordHash ""

  setUserHashAndSalt :: Text    -- ^ Salt
                     -> Text    -- ^ Password hash
                     -> user -> user
  setUserHashAndSalt =
      error "Define setSaltAndPasswordHash to get old-database compatibility"

  -- | Callback used in 'upgradePasswordHash' when compatibility is needed
  --   with old-style hashes (including ones already upgraded using
  --   'upgradePasswordHash').  This is not required for new applications,
  --   which do not have a separate salt field in user data: please define
  --   'setPasswordHash' instead.
  --
  --   The default implementation produces a runtime error, and will only be
  --   called if a non-empty salt value needs to be set for compatibility
  --   with an old database.
  setSaltAndPasswordHash :: Text    -- ^ Salt
                     -> Text    -- ^ Password hash
                     -> user -> user
  setSaltAndPasswordHash = setUserHashAndSalt

  {-# MINIMAL userPasswordHash, (setPasswordHash | (userPasswordSalt, setSaltAndPasswordHash)) #-}
{-# DEPRECATED setUserHashAndSalt "Please use setSaltAndPasswordHash instead" #-}


-- | Calculate salted hash using SHA1.  Retained for compatibility with
--   hashes in existing databases, but will not be used for new passwords.
saltedHash :: Text              -- ^ Salt
           -> Text              -- ^ Password
           -> Text
saltedHash salt = 
  pack . showDigest . sha1 . BL.pack . unpack . append salt

-- | Calculate a new-style password hash using "Crypto.PasswordStore".
passwordHash :: MonadIO m => Int -> Text -> m Text
passwordHash strength pwd = do
    h <- liftIO $ makePassword (BS.pack $ unpack pwd) strength
    return $ pack $ BS.unpack h

-- | Set password for user, using the given strength setting. Use this
--   function, or 'setPassword', to produce a user record containing the
--   hashed password.  Unlike previous versions of this module, no separate
--   salt field is required for new passwords (but it may still be required
--   for compatibility while old password hashes remain in the database).
setPasswordStrength :: (MonadIO m, HashDBUser user) => Int -> Text -> user -> m user
setPasswordStrength strength pwd u = do
    hashed <- passwordHash strength pwd
    return $ setPasswordHash hashed u

-- | As 'setPasswordStrength', but using the 'defaultStrength'
setPassword :: (MonadIO m, HashDBUser user) => Text -> user -> m user
setPassword = setPasswordStrength defaultStrength

-- | Upgrade existing user credentials to a stronger hash.  The existing
--   hash may have been produced either by previous versions of this module,
--   which used a weak algorithm, or from a weaker setting in the current
--   algorithm.  Use this function to produce an updated user record to
--   store in the database.
--
--   To allow transitional use, starting from hashes produced by older
--   versions of this module, and upgrading them to the new format,
--   we have to use the hash alone, without knowledge of the user's
--   plaintext password.  In this case, we apply the new algorithm to the
--   old hash, resulting in both hash functions, old and new, being used
--   one on top of the other; this situation is recognised by the hash
--   having the new format while the separate salt field is non-empty.
--
--   Returns Nothing if the user has no password (ie if 'userPasswordHash' u
--   is 'Nothing' and/or 'userPasswordSalt' u is 'Nothing').
upgradePasswordHash :: (MonadIO m, HashDBUser user) => Int -> user -> m (Maybe user)
upgradePasswordHash strength u = do
    let old = do h <- userPasswordHash u
                 s <- userPasswordSalt u
                 return (h, s)
    case old of
        Just (oldHash, oldSalt) -> do
            let oldHash' = BS.pack $ unpack oldHash
            if passwordStrength oldHash' > 0
              then
                -- Already a new-style hash, so only strengthen it as needed
                let newHash = pack $ BS.unpack $ strengthenPassword oldHash' strength
                in if oldSalt == ""
                   then return $ Just $ setPasswordHash newHash u
                   else return $ Just $ setSaltAndPasswordHash oldSalt newHash u
              else do
                -- Old-style hash: do extra layer of hash with the new algorithm
                newHash <- passwordHash strength oldHash
                return $ Just $ setSaltAndPasswordHash oldSalt newHash u
        Nothing -> return Nothing


----------------------------------------------------------------
-- Authentication
----------------------------------------------------------------

-- | Given a user ID and password in plaintext, validate them against
--   the database values.  This function retains compatibility with
--   databases containing hashes produced by previous versions of this
--   module, although they are less secure and should be upgraded as
--   soon as possible.  They can be upgraded using 'upgradePasswordHash',
--   or by insisting that users set new passwords.
validateUser :: ( YesodPersist yesod
                , b ~ YesodPersistBackend yesod
                , PersistMonadBackend (b (HandlerT yesod IO)) ~ PersistEntityBackend user
                , PersistUnique (b (HandlerT yesod IO))
                , PersistEntity user
                , HashDBUser    user
                ) => 
                Unique user     -- ^ User unique identifier
             -> Text            -- ^ Password in plaint-text
             -> HandlerT yesod IO Bool
validateUser userID passwd = do
  -- Checks that hash and password match
  let validate u = do hash <- userPasswordHash u
                      salt <- userPasswordSalt u
                      let hash' = BS.pack $ unpack hash
                          passwd' = BS.pack $ unpack
                              $ if salt == "" then passwd
                                else
                                    -- Extra layer for an upgraded old hash
                                    saltedHash salt passwd
                      if passwordStrength hash' > 0
                        -- Will give >0 for new-style hash, else fall back
                        then return $ verifyPassword passwd' hash'
                        else return $ hash == saltedHash salt passwd
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
              , PersistMonadBackend (b (HandlerT y IO)) ~ PersistEntityBackend user
              , PersistUnique (b (HandlerT y IO))
              )
           => (Text -> Maybe (Unique user))
           -> HandlerT Auth (HandlerT y IO) TypedContent
postLoginR uniq = do
    (mu,mp) <- lift $ runInputPost $ (,)
        <$> iopt textField "username"
        <*> iopt textField "password"

    isValid <- lift $ fromMaybe (return False) 
                 (validateUser <$> (uniq =<< mu) <*> mp)
    if isValid 
       then lift $ setCredsRedirect $ Creds "hashdb" (fromMaybe "" mu) []
       else do
           tm <- getRouteToParent
           lift $ loginErrorMessage (tm LoginR) "Invalid username/password"


-- | A drop in for the getAuthId method of your YesodAuth instance which
--   can be used if authHashDB is the only plugin in use.
getAuthIdHashDB :: ( YesodAuth master, YesodPersist master
                   , HashDBUser user, PersistEntity user
                   , Key user ~ AuthId master
                   , b ~ YesodPersistBackend master
                   , PersistMonadBackend (b (HandlerT master IO)) ~ PersistEntityBackend user
                   , PersistUnique (b (HandlerT master IO))
                   )
                => (AuthRoute -> Route master)   -- ^ your site's Auth Route
                -> (Text -> Maybe (Unique user)) -- ^ gets user ID
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
                Nothing       -> do
                                     _ <- loginErrorMessage (authR LoginR) "User not found"
                                     return Nothing

-- | Prompt for username and password, validate that against a database
--   which holds the username and a hash of the password
authHashDB :: ( YesodAuth m, YesodPersist m
              , HashDBUser user
              , PersistEntity user
              , b ~ YesodPersistBackend m
              , PersistMonadBackend (b (HandlerT m IO)) ~ PersistEntityBackend user
              , PersistUnique (b (HandlerT m IO)))
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
    deriving Typeable
|]

instance HashDBUser (UserGeneric backend) where
  userPasswordHash = Just . userPassword
  userPasswordSalt = Just . userSalt
  setSaltAndPasswordHash s h u = u { userSalt     = s
                               , userPassword = h
                               }
