{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Auth
    ( -- * Subsite
      Auth
    , AuthRoute
    , Route (..)
    , AuthPlugin (..)
    , getAuth
    , YesodAuth (..)
      -- * Plugin interface
    , Creds (..)
    , setCreds
    , clearCreds
      -- * User functions
    , defaultMaybeAuthId
    , maybeAuth
    , requireAuthId
    , requireAuth
      -- * Exception
    , AuthException (..)
    ) where

import Control.Monad                 (when)  
import Control.Monad.Trans.Maybe

import Data.Aeson
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as Map
import Network.HTTP.Conduit (Manager)

import Language.Haskell.TH.Syntax hiding (lift)

import qualified Network.Wai as W
import Text.Hamlet (shamlet)

import Yesod.Core
import Yesod.Persist
import Yesod.Json
import Yesod.Auth.Message (AuthMessage, defaultMessage)
import qualified Yesod.Auth.Message as Msg
import Yesod.Form (FormMessage)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

data Auth = Auth

type AuthRoute = Route Auth

type Method = Text
type Piece = Text

data AuthPlugin master = AuthPlugin
    { apName :: Text
    , apDispatch :: Method -> [Piece] -> GHandler Auth master ()
    , apLogin :: forall sub. (Route Auth -> Route master) -> GWidget sub master ()
    }

getAuth :: a -> Auth
getAuth = const Auth

-- | User credentials
data Creds master = Creds
    { credsPlugin :: Text -- ^ How the user was authenticated
    , credsIdent :: Text -- ^ Identifier. Exact meaning depends on plugin.
    , credsExtra :: [(Text, Text)]
    }

class (Yesod master, PathPiece (AuthId master), RenderMessage master FormMessage) => YesodAuth master where
    type AuthId master

    -- | Default destination on successful login, if no other
    -- destination exists.
    loginDest :: master -> Route master

    -- | Default destination on successful logout, if no other
    -- destination exists.
    logoutDest :: master -> Route master

    -- | Determine the ID associated with the set of credentials.
    getAuthId :: Creds master -> GHandler sub master (Maybe (AuthId master))

    -- | Which authentication backends to use.
    authPlugins :: master -> [AuthPlugin master]

    -- | What to show on the login page.
    loginHandler :: GHandler Auth master RepHtml
    loginHandler = defaultLayout $ do
        setTitleI Msg.LoginTitle
        tm <- lift getRouteToMaster
        master <- lift getYesod
        mapM_ (flip apLogin tm) (authPlugins master)

    -- | Used for i18n of messages provided by this package.
    renderAuthMessage :: master
                      -> [Text] -- ^ languages
                      -> AuthMessage -> Text
    renderAuthMessage _ _ = defaultMessage

    -- | After login and logout, redirect to the referring page, instead of
    -- 'loginDest' and 'logoutDest'. Default is 'False'.
    redirectToReferer :: master -> Bool
    redirectToReferer _ = False

    -- | Return an HTTP connection manager that is stored in the foundation
    -- type. This allows backends to reuse persistent connections. If none of
    -- the backends you're using use HTTP connections, you can safely return
    -- @error \"authHttpManager"@ here.
    authHttpManager :: master -> Manager

    -- | Called on a successful login. By default, calls
    -- @setMessageI NowLoggedIn@.
    onLogin :: GHandler sub master ()
    onLogin = setMessageI Msg.NowLoggedIn

    -- | Called on logout. By default, does nothing
    onLogout :: GHandler sub master ()
    onLogout = return ()

    -- | Retrieves user credentials, if user is authenticated.
    --
    -- By default, this calls 'defaultMaybeAuthId' to get the user ID from the
    -- session. This can be overridden to allow authentication via other means,
    -- such as checking for a special token in a request header. This is
    -- especially useful for creating an API to be accessed via some means
    -- other than a browser.
    --
    -- Since 1.1.2
    maybeAuthId :: GHandler sub master (Maybe (AuthId master))
    maybeAuthId = defaultMaybeAuthId

credsKey :: Text
credsKey = "_ID"

-- | Retrieves user credentials from the session, if user is authenticated.
--
-- Since 1.1.2
defaultMaybeAuthId :: YesodAuth master
                   => GHandler sub master (Maybe (AuthId master))
defaultMaybeAuthId = do
    ms <- lookupSession credsKey
    case ms of
        Nothing -> return Nothing
        Just s -> return $ fromPathPiece s

mkYesodSub "Auth"
    [ ClassP ''YesodAuth [VarT $ mkName "master"]
    ]
#define STRINGS *Texts
    [parseRoutes|
/check                 CheckR      GET
/login                 LoginR      GET
/logout                LogoutR     GET POST
/page/#Text/STRINGS PluginR
|]

-- | Sets user credentials for the session after checking them with authentication backends.
setCreds :: YesodAuth master
         => Bool         -- ^ if HTTP redirects should be done
         -> Creds master -- ^ new credentials
         -> GHandler sub master ()
setCreds doRedirects creds = do
    y    <- getYesod
    maid <- getAuthId creds
    case maid of
        Nothing ->
          when doRedirects $ do
            case authRoute y of
              Nothing -> do rh <- defaultLayout $ toWidget [shamlet|
$newline never
<h1>Invalid login
|]
                            sendResponse rh
              Just ar -> do setMessageI Msg.InvalidLogin
                            redirect ar
        Just aid -> do
            setSession credsKey $ toPathPiece aid
            when doRedirects $ do
              onLogin
              redirectUltDest $ loginDest y

-- | Clears current user credentials for the session.
--
-- Since 1.1.7
clearCreds :: YesodAuth master
           => Bool -- ^ if HTTP redirect to 'logoutDest' should be done
           -> GHandler sub master ()
clearCreds doRedirects = do
    y <- getYesod
    deleteSession credsKey
    when doRedirects $ do
        onLogout
        redirectUltDest $ logoutDest y

getCheckR :: YesodAuth master => GHandler Auth master RepHtmlJson
getCheckR = do
    creds <- maybeAuthId
    defaultLayoutJson (do
        setTitle "Authentication Status"
        toWidget $ html' creds) (jsonCreds creds)
  where
    html' creds =
        [shamlet|
$newline never
<h1>Authentication Status
$maybe _ <- creds
    <p>Logged in.
$nothing
    <p>Not logged in.
|]
    jsonCreds creds =
        Object $ Map.fromList
            [ (T.pack "logged_in", Bool $ maybe False (const True) creds)
            ]

setUltDestReferer' :: YesodAuth master => GHandler sub master ()
setUltDestReferer' = do
    master <- getYesod
    when (redirectToReferer master) setUltDestReferer

getLoginR :: YesodAuth master => GHandler Auth master RepHtml
getLoginR = setUltDestReferer' >> loginHandler

getLogoutR :: YesodAuth master => GHandler Auth master ()
getLogoutR = do
    tm <- getRouteToMaster
    setUltDestReferer' >> redirectToPost (tm LogoutR)

postLogoutR :: YesodAuth master => GHandler Auth master ()
postLogoutR = clearCreds True

handlePluginR :: YesodAuth master => Text -> [Text] -> GHandler Auth master ()
handlePluginR plugin pieces = do
    master <- getYesod
    env <- waiRequest
    let method = decodeUtf8With lenientDecode $ W.requestMethod env
    case filter (\x -> apName x == plugin) (authPlugins master) of
        [] -> notFound
        ap:_ -> apDispatch ap method pieces

maybeAuth :: ( YesodAuth master
#if MIN_VERSION_persistent(1, 1, 0)
             , PersistMonadBackend (b (GHandler sub master)) ~ PersistEntityBackend val
             , b ~ YesodPersistBackend master
             , Key val ~ AuthId master
             , PersistStore (b (GHandler sub master))
#else
             , b ~ YesodPersistBackend master
             , b ~ PersistEntityBackend val
             , Key b val ~ AuthId master
             , PersistStore b (GHandler sub master)
#endif
             , PersistEntity val
             , YesodPersist master
             ) => GHandler sub master (Maybe (Entity val))
maybeAuth = runMaybeT $ do
    aid <- MaybeT $ maybeAuthId
    a   <- MaybeT $ runDB $ get aid
    return $ Entity aid a

requireAuthId :: YesodAuth master => GHandler sub master (AuthId master)
requireAuthId = maybeAuthId >>= maybe redirectLogin return

requireAuth :: ( YesodAuth master
               , b ~ YesodPersistBackend master
#if MIN_VERSION_persistent(1, 1, 0)
               , PersistMonadBackend (b (GHandler sub master)) ~ PersistEntityBackend val
               , Key val ~ AuthId master
               , PersistStore (b (GHandler sub master))
#else
               , b ~ PersistEntityBackend val
               , Key b val ~ AuthId master
               , PersistStore b (GHandler sub master)
#endif
               , PersistEntity val
               , YesodPersist master
               ) => GHandler sub master (Entity val)
requireAuth = maybeAuth >>= maybe redirectLogin return

redirectLogin :: Yesod master => GHandler sub master a
redirectLogin = do
    y <- getYesod
    setUltDestCurrent
    case authRoute y of
        Just z -> redirect z
        Nothing -> permissionDenied "Please configure authRoute"

instance YesodAuth master => RenderMessage master AuthMessage where
    renderMessage = renderAuthMessage

data AuthException = InvalidBrowserIDAssertion
                   | InvalidFacebookResponse
    deriving (Show, Typeable)
instance Exception AuthException
