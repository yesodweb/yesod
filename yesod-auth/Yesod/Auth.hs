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

data AuthPlugin m = AuthPlugin
    { apName :: Text
    , apDispatch :: Method -> [Piece] -> GHandler Auth m ()
    , apLogin :: forall s. (Route Auth -> Route m) -> GWidget s m ()
    }

getAuth :: a -> Auth
getAuth = const Auth

-- | User credentials
data Creds m = Creds
    { credsPlugin :: Text -- ^ How the user was authenticated
    , credsIdent :: Text -- ^ Identifier. Exact meaning depends on plugin.
    , credsExtra :: [(Text, Text)]
    }

class (Yesod m, PathPiece (AuthId m), RenderMessage m FormMessage) => YesodAuth m where
    type AuthId m

    -- | Default destination on successful login, if no other
    -- destination exists.
    loginDest :: m -> Route m

    -- | Default destination on successful logout, if no other
    -- destination exists.
    logoutDest :: m -> Route m

    -- | Determine the ID associated with the set of credentials.
    getAuthId :: Creds m -> GHandler s m (Maybe (AuthId m))

    -- | Which authentication backends to use.
    authPlugins :: m -> [AuthPlugin m]

    -- | What to show on the login page.
    loginHandler :: GHandler Auth m RepHtml
    loginHandler = defaultLayout $ do
        setTitleI Msg.LoginTitle
        tm <- lift getRouteToMaster
        master <- lift getYesod
        mapM_ (flip apLogin tm) (authPlugins master)

    -- | Used for i18n of messages provided by this package.
    renderAuthMessage :: m
                      -> [Text] -- ^ languages
                      -> AuthMessage -> Text
    renderAuthMessage _ _ = defaultMessage

    -- | After login and logout, redirect to the referring page, instead of
    -- 'loginDest' and 'logoutDest'. Default is 'False'.
    redirectToReferer :: m -> Bool
    redirectToReferer _ = False

    -- | Return an HTTP connection manager that is stored in the foundation
    -- type. This allows backends to reuse persistent connections. If none of
    -- the backends you're using use HTTP connections, you can safely return
    -- @error \"authHttpManager"@ here.
    authHttpManager :: m -> Manager

    -- | Called on a successful login. By default, calls
    -- @setMessageI NowLoggedIn@.
    onLogin :: GHandler s m ()
    onLogin = setMessageI Msg.NowLoggedIn

    -- | Called on logout. By default, does nothing
    onLogout :: GHandler s m ()
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
    maybeAuthId :: GHandler s m (Maybe (AuthId m))
    maybeAuthId = defaultMaybeAuthId

credsKey :: Text
credsKey = "_ID"

-- | Retrieves user credentials from the session, if user is authenticated.
--
-- Since 1.1.2
defaultMaybeAuthId :: YesodAuth m => GHandler s m (Maybe (AuthId m))
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

setCreds :: YesodAuth m => Bool -> Creds m -> GHandler s m ()
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

getCheckR :: YesodAuth m => GHandler Auth m RepHtmlJson
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
    m <- getYesod
    when (redirectToReferer m) setUltDestReferer

getLoginR :: YesodAuth m => GHandler Auth m RepHtml
getLoginR = setUltDestReferer' >> loginHandler

getLogoutR :: YesodAuth m => GHandler Auth m ()
getLogoutR = do
    tm <- getRouteToMaster
    setUltDestReferer' >> redirectToPost (tm LogoutR)

postLogoutR :: YesodAuth m => GHandler Auth m ()
postLogoutR = do
    y <- getYesod
    deleteSession credsKey
    onLogout
    redirectUltDest $ logoutDest y

handlePluginR :: YesodAuth m => Text -> [Text] -> GHandler Auth m ()
handlePluginR plugin pieces = do
    master <- getYesod
    env <- waiRequest
    let method = decodeUtf8With lenientDecode $ W.requestMethod env
    case filter (\x -> apName x == plugin) (authPlugins master) of
        [] -> notFound
        ap:_ -> apDispatch ap method pieces

maybeAuth :: ( YesodAuth m
#if MIN_VERSION_persistent(1, 1, 0)
             , PersistMonadBackend (b (GHandler s m)) ~ PersistEntityBackend val
             , b ~ YesodPersistBackend m
             , Key val ~ AuthId m
             , PersistStore (b (GHandler s m))
#else
             , b ~ YesodPersistBackend m
             , b ~ PersistEntityBackend val
             , Key b val ~ AuthId m
             , PersistStore b (GHandler s m)
#endif
             , PersistEntity val
             , YesodPersist m
             ) => GHandler s m (Maybe (Entity val))
maybeAuth = runMaybeT $ do
    aid <- MaybeT $ maybeAuthId
    a   <- MaybeT $ runDB $ get aid
    return $ Entity aid a

requireAuthId :: YesodAuth m => GHandler s m (AuthId m)
requireAuthId = maybeAuthId >>= maybe redirectLogin return

requireAuth :: ( YesodAuth m
               , b ~ YesodPersistBackend m
#if MIN_VERSION_persistent(1, 1, 0)
               , PersistMonadBackend (b (GHandler s m)) ~ PersistEntityBackend val
               , Key val ~ AuthId m
               , PersistStore (b (GHandler s m))
#else
               , b ~ PersistEntityBackend val
               , Key b val ~ AuthId m
               , PersistStore b (GHandler s m)
#endif
               , PersistEntity val
               , YesodPersist m
               ) => GHandler s m (Entity val)
requireAuth = maybeAuth >>= maybe redirectLogin return

redirectLogin :: Yesod m => GHandler s m a
redirectLogin = do
    y <- getYesod
    setUltDestCurrent
    case authRoute y of
        Just z -> redirect z
        Nothing -> permissionDenied "Please configure authRoute"

instance YesodAuth m => RenderMessage m AuthMessage where
    renderMessage = renderAuthMessage

data AuthException = InvalidBrowserIDAssertion
                   | InvalidFacebookResponse
    deriving (Show, Typeable)
instance Exception AuthException
