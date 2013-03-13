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

import Yesod.Auth.Routes
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
import Yesod.Auth.Message (AuthMessage, defaultMessage)
import qualified Yesod.Auth.Message as Msg
import Yesod.Form (FormMessage)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

type AuthRoute = Route Auth

type AuthHandler master a = YesodAuth master => HandlerT Auth (GHandler master) a

type Method = Text
type Piece = Text

data AuthPlugin master = AuthPlugin
    { apName :: Text
    , apDispatch :: Method -> [Piece] -> AuthHandler master ()
    , apLogin :: (Route Auth -> Text) -> GWidget master ()
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
    getAuthId :: Creds master -> GHandler master (Maybe (AuthId master))

    -- | Which authentication backends to use.
    authPlugins :: master -> [AuthPlugin master]

    -- | What to show on the login page.
    loginHandler :: AuthHandler master RepHtml
    loginHandler = do
        render <- getUrlRender
        lift $ defaultLayout $ do
            setTitleI Msg.LoginTitle
            master <- lift getYesod
            mapM_ (flip apLogin render) (authPlugins master)

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
    onLogin :: GHandler master ()
    onLogin = setMessageI Msg.NowLoggedIn

    -- | Called on logout. By default, does nothing
    onLogout :: GHandler master ()
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
    maybeAuthId :: GHandler master (Maybe (AuthId master))
    maybeAuthId = defaultMaybeAuthId

credsKey :: Text
credsKey = "_ID"

-- | Retrieves user credentials from the session, if user is authenticated.
--
-- Since 1.1.2
defaultMaybeAuthId :: YesodAuth master
                   => GHandler master (Maybe (AuthId master))
defaultMaybeAuthId = do
    ms <- lookupSession credsKey
    case ms of
        Nothing -> return Nothing
        Just s -> return $ fromPathPiece s

setCreds :: Bool -> Creds master -> AuthHandler master ()
setCreds doRedirects creds = lift $ do
    y    <- getYesod
    maid <- getAuthId creds
    case maid of
        Nothing -> when doRedirects $ do
            case authRoute y of
                Nothing -> do
                    res <- selectRep $ do
                        provideRep $ defaultLayout $ toWidget [shamlet|<h1>Invalid login|]
                        provideRep $ return $ object ["message" .= ("Invalid Login" :: Text)]
                    sendResponse res
                Just ar -> do
                    res <- selectRep $ do
                        provideRepType typeHtml $ do
                            setMessageI Msg.InvalidLogin
                            _ <- redirect ar
                            return ()
                        provideRep $ return $ object ["message" .= ("Invalid Login" :: Text)]
                    sendResponse res
        Just aid -> do
            setSession credsKey $ toPathPiece aid
            when doRedirects $ do
                onLogin
                res <- selectRep $ do
                    provideRepType typeHtml $ do
                        _ <- redirectUltDest $ loginDest y
                        return ()
                    provideRep $ return $ object ["message" .= ("Login Successful" :: Text)]
                sendResponse res

getCheckR :: AuthHandler master TypedContent
getCheckR = lift $ do
    creds <- maybeAuthId
    defaultLayoutJson (do
        setTitle "Authentication Status"
        toWidget $ html' creds) (return $ jsonCreds creds)
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

setUltDestReferer' :: AuthHandler master ()
setUltDestReferer' = lift $ do
    master <- getYesod
    when (redirectToReferer master) setUltDestReferer

getLoginR :: AuthHandler master RepHtml
getLoginR = setUltDestReferer' >> loginHandler

getLogoutR :: AuthHandler master ()
getLogoutR = setUltDestReferer' >> redirectToPost LogoutR

postLogoutR :: AuthHandler master ()
postLogoutR = lift $ do
    y <- getYesod
    deleteSession credsKey
    onLogout
    redirectUltDest $ logoutDest y

handlePluginR :: Text -> [Text] -> AuthHandler master ()
handlePluginR plugin pieces = do
    master <- lift getYesod
    env <- waiRequest
    let method = decodeUtf8With lenientDecode $ W.requestMethod env
    case filter (\x -> apName x == plugin) (authPlugins master) of
        [] -> notFound
        ap:_ -> apDispatch ap method pieces

maybeAuth :: ( YesodAuth master
             , PersistMonadBackend (b (GHandler master)) ~ PersistEntityBackend val
             , b ~ YesodPersistBackend master
             , Key val ~ AuthId master
             , PersistStore (b (GHandler master))
             , PersistEntity val
             , YesodPersist master
             , Typeable val
             ) => GHandler master (Maybe (Entity val))
maybeAuth = runMaybeT $ do
    aid <- MaybeT $ maybeAuthId
    a   <- MaybeT
         $ fmap unCachedMaybeAuth
         $ cached
         $ fmap CachedMaybeAuth
         $ runDB
         $ get aid
    return $ Entity aid a

newtype CachedMaybeAuth val = CachedMaybeAuth { unCachedMaybeAuth :: Maybe val }
    deriving Typeable

requireAuthId :: YesodAuth master => GHandler master (AuthId master)
requireAuthId = maybeAuthId >>= maybe redirectLogin return

requireAuth :: ( YesodAuth master
               , b ~ YesodPersistBackend master
               , PersistMonadBackend (b (GHandler master)) ~ PersistEntityBackend val
               , Key val ~ AuthId master
               , PersistStore (b (GHandler master))
               , PersistEntity val
               , YesodPersist master
               , Typeable val
               ) => GHandler master (Entity val)
requireAuth = maybeAuth >>= maybe redirectLogin return

redirectLogin :: Yesod master => GHandler master a
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

instance YesodAuth master => YesodSubDispatch Auth (GHandler master) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuth)
