{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
    , YesodAuthPersist
    , AuthEntity
      -- * Plugin interface
    , Creds (..)
    , setCreds
    , clearCreds
    , loginErrorMessage
    , loginErrorMessageI
      -- * User functions
    , defaultMaybeAuthId
    , maybeAuth
    , requireAuthId
    , requireAuth
      -- * Exception
    , AuthException (..)
      -- * Helper
    , AuthHandler
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
import Data.Monoid (Endo)
import Network.HTTP.Conduit (Manager)

import qualified Network.Wai as W
import Text.Hamlet (shamlet)

import Yesod.Core
import Yesod.Persist
import Yesod.Auth.Message (AuthMessage, defaultMessage)
import qualified Yesod.Auth.Message as Msg
import Yesod.Form (FormMessage)
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Network.HTTP.Types          (unauthorized401)
import Control.Monad.Trans.Resource (MonadResourceBase)
import qualified Control.Monad.Trans.Writer    as Writer

type AuthRoute = Route Auth

type AuthHandler master a = YesodAuth master => HandlerT Auth (HandlerT master IO) a

type Method = Text
type Piece = Text

data AuthPlugin master = AuthPlugin
    { apName :: Text
    , apDispatch :: Method -> [Piece] -> AuthHandler master ()
    , apLogin :: (Route Auth -> Route master) -> WidgetT master IO ()
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
    getAuthId :: Creds master -> HandlerT master IO (Maybe (AuthId master))

    -- | Which authentication backends to use.
    authPlugins :: master -> [AuthPlugin master]

    -- | What to show on the login page.
    loginHandler :: AuthHandler master RepHtml
    loginHandler = do
        tp <- getRouteToParent
        lift $ defaultLayout $ do
            setTitleI Msg.LoginTitle
            master <- getYesod
            mapM_ (flip apLogin tp) (authPlugins master)

    -- | Used for i18n of messages provided by this package.
    renderAuthMessage :: master
                      -> [Text] -- ^ languages
                      -> AuthMessage
                      -> Text
    renderAuthMessage _ _ = defaultMessage

    -- | After login and logout, redirect to the referring page, instead of
    -- 'loginDest' and 'logoutDest'. Default is 'False'.
    redirectToReferer :: master -> Bool
    redirectToReferer _ = False

    -- | Return an HTTP connection manager that is stored in the foundation
    -- type. This allows backends to reuse persistent connections. If none of
    -- the backends you're using use HTTP connections, you can safely return
    -- @error \"authHttpManager\"@ here.
    authHttpManager :: master -> Manager

    -- | Called on a successful login. By default, calls
    -- @setMessageI NowLoggedIn@.
    onLogin :: HandlerT master IO ()
    onLogin = setMessageI Msg.NowLoggedIn

    -- | Called on logout. By default, does nothing
    onLogout :: HandlerT master IO ()
    onLogout = return ()

    -- | Retrieves user credentials, if user is authenticated.
    --
    -- By default, this calls 'defaultMaybeAuthId' to get the user ID from the
    -- session. This can be overridden to allow authentication via other means,
    -- such as checking for a special token in a request header. This is
    -- especially useful for creating an API to be accessed via some means
    -- other than a browser.
    --
    -- Since 1.2.0
    maybeAuthId :: HandlerT master IO (Maybe (AuthId master))

    default maybeAuthId
        :: ( YesodAuth master
           , PersistMonadBackend (b (HandlerT master IO)) ~ PersistEntityBackend val
           , b ~ YesodPersistBackend master
           , Key val ~ AuthId master
           , PersistStore (b (HandlerT master IO))
           , PersistEntity val
           , YesodPersist master
           , Typeable val
           )
        => HandlerT master IO (Maybe (AuthId master))
    maybeAuthId = defaultMaybeAuthId

    -- | Similar to 'maybeAuthId', but redirects to a login page if user is not
    -- authenticated.
    --
    -- Since 1.1.0
    requireAuthId :: YesodAuthPersist master => HandlerT master IO (AuthId master)
    requireAuthId = maybeAuthId >>= maybe redirectLogin return

    requireAuth :: YesodAuthPersist master => HandlerT master IO (Entity (AuthEntity master))
    requireAuth = maybeAuth >>= maybe redirectLogin return

credsKey :: Text
credsKey = "_ID"

-- | Retrieves user credentials from the session, if user is authenticated.
--
-- This function does /not/ confirm that the credentials are valid, see
-- 'maybeAuthIdRaw' for more information.
--
-- Since 1.1.2
defaultMaybeAuthId
          :: ( YesodAuth master
             , PersistMonadBackend (b (HandlerT master IO)) ~ PersistEntityBackend val
             , b ~ YesodPersistBackend master
             , Key val ~ AuthId master
             , PersistStore (b (HandlerT master IO))
             , PersistEntity val
             , YesodPersist master
             , Typeable val
             ) => HandlerT master IO (Maybe (AuthId master))
defaultMaybeAuthId = do
    ms <- lookupSession credsKey
    case ms of
        Nothing -> return Nothing
        Just s ->
            case fromPathPiece s of
                Nothing -> return Nothing
                Just aid -> fmap (fmap entityKey) $ cachedAuth aid

cachedAuth :: ( YesodAuth master
             , PersistMonadBackend (b (HandlerT master IO)) ~ PersistEntityBackend val
             , b ~ YesodPersistBackend master
             , Key val ~ AuthId master
             , PersistStore (b (HandlerT master IO))
             , PersistEntity val
             , YesodPersist master
             , Typeable val
             ) => AuthId master -> HandlerT master IO (Maybe (Entity val))
cachedAuth aid = runMaybeT $ do
    a <- MaybeT $ fmap unCachedMaybeAuth
                $ cached
                $ fmap CachedMaybeAuth
                $ runDB
                $ get aid
    return $ Entity aid a


loginErrorMessageI :: (MonadResourceBase m, YesodAuth master)
                   => Route child
                   -> AuthMessage
                   -> HandlerT child (HandlerT master m) a
loginErrorMessageI dest msg = do
  toParent <- getRouteToParent
  lift $ loginErrorMessageMasterI (toParent dest) msg


loginErrorMessageMasterI :: (YesodAuth master, MonadResourceBase m, RenderMessage master AuthMessage)
         => Route master
         -> AuthMessage
         -> HandlerT master m a
loginErrorMessageMasterI dest msg = do
  mr <- getMessageRender
  loginErrorMessage dest (mr msg)

-- | For HTML, set the message and redirect to the route.
-- For JSON, send the message and a 401 status
loginErrorMessage :: MonadResourceBase m
         => Route site
         -> Text
         -> HandlerT site m a
loginErrorMessage dest msg =
  sendResponseStatus unauthorized401 =<< (
    selectRep $ do
      provideRep $ do
          setMessage $ toHtml msg
          fmap asHtml $ redirect dest
      provideJsonMessage msg
  )
  where
    asHtml :: Html -> Html
    asHtml = id

provideJsonMessage :: Monad m => Text -> Writer.Writer (Endo [ProvidedRep m]) ()
provideJsonMessage msg = provideRep $ return $ object ["message" .= msg]


-- | Sets user credentials for the session after checking them with authentication backends.
setCreds :: YesodAuth master
         => Bool         -- ^ if HTTP redirects should be done
         -> Creds master -- ^ new credentials
         -> HandlerT master IO ()
setCreds doRedirects creds = do
    y    <- getYesod
    maid <- getAuthId creds
    case maid of
        Nothing -> when doRedirects $ do
            case authRoute y of
                Nothing -> do
                    sendResponseStatus unauthorized401 =<< (
                      selectRep $ do
                        provideRep $ defaultLayout $ toWidget [shamlet|<h1>Invalid login|]
                        provideJsonMessage "Invalid Login"
                      )
                Just ar -> loginErrorMessageMasterI ar Msg.InvalidLogin
        Just aid -> do
            setSession credsKey $ toPathPiece aid
            when doRedirects $ do
              onLogin
              res <- selectRep $ do
                  provideRepType typeHtml $ do
                      _ <- redirectUltDest $ loginDest y
                      return ()
                  provideJsonMessage "Login Successful"
              sendResponse res

-- | Clears current user credentials for the session.
--
-- Since 1.1.7
clearCreds :: YesodAuth master
           => Bool -- ^ if HTTP redirect to 'logoutDest' should be done
           -> HandlerT master IO ()
clearCreds doRedirects = do
    y <- getYesod
    deleteSession credsKey
    when doRedirects $ do
        onLogout
        redirectUltDest $ logoutDest y

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
postLogoutR = lift $ clearCreds True

handlePluginR :: Text -> [Text] -> AuthHandler master ()
handlePluginR plugin pieces = do
    master <- lift getYesod
    env <- waiRequest
    let method = decodeUtf8With lenientDecode $ W.requestMethod env
    case filter (\x -> apName x == plugin) (authPlugins master) of
        [] -> notFound
        ap:_ -> apDispatch ap method pieces

maybeAuth :: ( YesodAuth master
             , PersistMonadBackend (b (HandlerT master IO)) ~ PersistEntityBackend val
             , b ~ YesodPersistBackend master
             , Key val ~ AuthId master
             , PersistStore (b (HandlerT master IO))
             , PersistEntity val
             , YesodPersist master
             , Typeable val
             ) => HandlerT master IO (Maybe (Entity val))
maybeAuth = runMaybeT $ do
    aid <- MaybeT maybeAuthId
    MaybeT $ cachedAuth aid

newtype CachedMaybeAuth val = CachedMaybeAuth { unCachedMaybeAuth :: Maybe val }
    deriving Typeable

-- | Constraint which states that the given site is an instance of @YesodAuth@
-- and that its @AuthId@ is in fact a persistent @Key@ for the given value.
-- This is the common case in Yesod, and means that you can easily look up the
-- full informatin on a given user.
--
-- Since 1.2.0
type YesodAuthPersist master =
    ( YesodAuth master
    , PersistMonadBackend (YesodPersistBackend master (HandlerT master IO))
        ~ PersistEntityBackend (AuthEntity master)
    , Key (AuthEntity master) ~ AuthId master
    , PersistStore (YesodPersistBackend master (HandlerT master IO))
    , PersistEntity (AuthEntity master)
    , YesodPersist master
    , Typeable (AuthEntity master)
    )

-- | If the @AuthId@ for a given site is a persistent ID, this will give the
-- value for that entity. E.g.:
--
-- > type AuthId MySite = UserId
-- > AuthEntity MySite ~ User
--
-- Since 1.2.0
type AuthEntity master = KeyEntity (AuthId master)


redirectLogin :: Yesod master => HandlerT master IO a
redirectLogin = do
    y <- getYesod
    setUltDestCurrent
    case authRoute y of
        Just z -> redirect z
        Nothing -> permissionDenied "Please configure authRoute"

instance YesodAuth master => RenderMessage master AuthMessage where
    renderMessage = renderAuthMessage

data AuthException = InvalidFacebookResponse
    deriving (Show, Typeable)
instance Exception AuthException

instance YesodAuth master => YesodSubDispatch Auth (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuth)
