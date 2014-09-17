{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Auth
    ( -- * Subsite
      Auth
    , AuthRoute
    , Route (..)
    , AuthPlugin (..)
    , getAuth
    , YesodAuth (..)
    , YesodAuthPersist (..)
      -- * Plugin interface
    , Creds (..)
    , setCreds
    , setCredsRedirect
    , clearCreds
    , loginErrorMessage
    , loginErrorMessageI
      -- * User functions
    , defaultMaybeAuthId
    , maybeAuthPair
    , maybeAuth
    , requireAuthId
    , requireAuthPair
    , requireAuth
      -- * Exception
    , AuthException (..)
      -- * Helper
    , AuthHandler
      -- * Internal
    , credsKey
    , provideJsonMessage
    , messageJson401
    , asHtml
    ) where

import Control.Monad                 (when)
import Control.Monad.Trans.Maybe

import Yesod.Auth.Routes
import Data.Aeson hiding (json)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as Map
import Data.Monoid (Endo)
import Network.HTTP.Conduit (Manager)

import qualified Network.Wai as W

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
import Control.Monad (void)

type AuthRoute = Route Auth

type AuthHandler master a = YesodAuth master => HandlerT Auth (HandlerT master IO) a

type Method = Text
type Piece = Text

data AuthPlugin master = AuthPlugin
    { apName :: Text
    , apDispatch :: Method -> [Piece] -> AuthHandler master TypedContent
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

    -- | specify the layout. Uses defaultLayout by default
    authLayout :: WidgetT master IO () -> HandlerT master IO Html
    authLayout = defaultLayout

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
    loginHandler :: AuthHandler master Html
    loginHandler = do
        tp <- getRouteToParent
        lift $ authLayout $ do
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
        :: (YesodAuthPersist master, Typeable (AuthEntity master))
        => HandlerT master IO (Maybe (AuthId master))
    maybeAuthId = defaultMaybeAuthId

    -- | Called on login error for HTTP requests. By default, calls
    -- @setMessage@ and redirects to @dest@.
    onErrorHtml :: (MonadResourceBase m) => Route master -> Text -> HandlerT master m Html
    onErrorHtml dest msg = do
        setMessage $ toHtml msg
        fmap asHtml $ redirect dest

-- | Internal session key used to hold the authentication information.
--
-- Since 1.2.3
credsKey :: Text
credsKey = "_ID"

-- | Retrieves user credentials from the session, if user is authenticated.
--
-- This function does /not/ confirm that the credentials are valid, see
-- 'maybeAuthIdRaw' for more information.
--
-- Since 1.1.2
defaultMaybeAuthId
    :: (YesodAuthPersist master, Typeable (AuthEntity master))
    => HandlerT master IO (Maybe (AuthId master))
defaultMaybeAuthId = runMaybeT $ do
    s   <- MaybeT $ lookupSession credsKey
    aid <- MaybeT $ return $ fromPathPiece s
    _   <- MaybeT $ cachedAuth aid
    return aid

cachedAuth
    :: (YesodAuthPersist master, Typeable (AuthEntity master))
    => AuthId master -> HandlerT master IO (Maybe (AuthEntity master))
cachedAuth
    = fmap unCachedMaybeAuth
    . cached
    . fmap CachedMaybeAuth
    . getAuthEntity


loginErrorMessageI :: (MonadResourceBase m, YesodAuth master)
                   => Route child
                   -> AuthMessage
                   -> HandlerT child (HandlerT master m) TypedContent
loginErrorMessageI dest msg = do
  toParent <- getRouteToParent
  lift $ loginErrorMessageMasterI (toParent dest) msg


loginErrorMessageMasterI :: (YesodAuth master, MonadResourceBase m, RenderMessage master AuthMessage)
         => Route master
         -> AuthMessage
         -> HandlerT master m TypedContent
loginErrorMessageMasterI dest msg = do
  mr <- getMessageRender
  loginErrorMessage dest (mr msg)

-- | For HTML, set the message and redirect to the route.
-- For JSON, send the message and a 401 status
loginErrorMessage :: (YesodAuth master, MonadResourceBase m)
         => Route master
         -> Text
         -> HandlerT master m TypedContent
loginErrorMessage dest msg = messageJson401 msg (onErrorHtml dest msg)

messageJson401 :: MonadResourceBase m => Text -> HandlerT master m Html -> HandlerT master m TypedContent
messageJson401 msg html = selectRep $ do
    provideRep html
    provideRep $ do
        let obj = object ["message" .= msg]
        void $ sendResponseStatus unauthorized401 obj
        return obj

provideJsonMessage :: Monad m => Text -> Writer.Writer (Endo [ProvidedRep m]) ()
provideJsonMessage msg = provideRep $ return $ object ["message" .= msg]


setCredsRedirect :: YesodAuth master
         => Creds master -- ^ new credentials
         -> HandlerT master IO TypedContent
setCredsRedirect creds = do
    y    <- getYesod
    maid <- getAuthId creds
    case maid of
        Nothing ->
            case authRoute y of
                Nothing -> do
                    messageJson401 "Invalid Login" $ authLayout $
                        toWidget [shamlet|<h1>Invalid login|]
                Just ar -> loginErrorMessageMasterI ar Msg.InvalidLogin
        Just aid -> do
            setSession credsKey $ toPathPiece aid
            onLogin
            res <- selectRep $ do
                provideRepType typeHtml $
                    fmap asHtml $ redirectUltDest $ loginDest y
                provideJsonMessage "Login Successful"
            sendResponse res

-- | Sets user credentials for the session after checking them with authentication backends.
setCreds :: YesodAuth master
         => Bool         -- ^ if HTTP redirects should be done
         -> Creds master -- ^ new credentials
         -> HandlerT master IO ()
setCreds doRedirects creds =
    if doRedirects
      then void $ setCredsRedirect creds
      else do maid <- getAuthId creds
              case maid of
                  Nothing -> return ()
                  Just aid -> setSession credsKey $ toPathPiece aid

-- | same as defaultLayoutJson, but uses authLayout
authLayoutJson :: (YesodAuth site, ToJSON j)
                  => WidgetT site IO ()  -- ^ HTML
                  -> HandlerT site IO j  -- ^ JSON
                  -> HandlerT site IO TypedContent
authLayoutJson w json = selectRep $ do
    provideRep $ authLayout w
    provideRep $ fmap toJSON json

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
    authLayoutJson (do
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

getLoginR :: AuthHandler master Html
getLoginR = setUltDestReferer' >> loginHandler

getLogoutR :: AuthHandler master ()
getLogoutR = setUltDestReferer' >> redirectToPost LogoutR

postLogoutR :: AuthHandler master ()
postLogoutR = lift $ clearCreds True

handlePluginR :: Text -> [Text] -> AuthHandler master TypedContent
handlePluginR plugin pieces = do
    master <- lift getYesod
    env <- waiRequest
    let method = decodeUtf8With lenientDecode $ W.requestMethod env
    case filter (\x -> apName x == plugin) (authPlugins master) of
        [] -> notFound
        ap:_ -> apDispatch ap method pieces

-- | Similar to 'maybeAuthId', but additionally look up the value associated
-- with the user\'s database identifier to get the value in the database. This
-- assumes that you are using a Persistent database.
--
-- Since 1.1.0
maybeAuth :: ( YesodAuthPersist master
             , val ~ AuthEntity master
             , Key val ~ AuthId master
             , PersistEntity val
             , Typeable val
             ) => HandlerT master IO (Maybe (Entity val))
maybeAuth = runMaybeT $ do
    (aid, ae) <- MaybeT maybeAuthPair
    return $ Entity aid ae

-- | Similar to 'maybeAuth', but doesn’t assume that you are using a
-- Persistent database.
--
-- Since 1.4.0
maybeAuthPair :: (YesodAuthPersist master, Typeable (AuthEntity master))
              => HandlerT master IO (Maybe (AuthId master, AuthEntity master))
maybeAuthPair = runMaybeT $ do
    aid <- MaybeT maybeAuthId
    ae  <- MaybeT $ cachedAuth aid
    return (aid, ae)


newtype CachedMaybeAuth val = CachedMaybeAuth { unCachedMaybeAuth :: Maybe val }
    deriving Typeable

-- | Class which states that the given site is an instance of @YesodAuth@
-- and that its @AuthId@ is a lookup key for the full user information in
-- a @YesodPersist@ database.
--
-- The default implementation of @getAuthEntity@ assumes that the @AuthId@
-- for the @YesodAuth@ superclass is in fact a persistent @Key@ for the
-- given value.  This is the common case in Yesod, and means that you can
-- easily look up the full information on a given user.
--
-- Since 1.4.0
class (YesodAuth master, YesodPersist master) => YesodAuthPersist master where
    -- | If the @AuthId@ for a given site is a persistent ID, this will give the
    -- value for that entity. E.g.:
    --
    -- > type AuthId MySite = UserId
    -- > AuthEntity MySite ~ User
    --
    -- Since 1.2.0
    type AuthEntity master :: *
    type AuthEntity master = KeyEntity (AuthId master)

    getAuthEntity :: AuthId master -> HandlerT master IO (Maybe (AuthEntity master))

    default getAuthEntity
        :: ( YesodPersistBackend master
               ~ PersistEntityBackend (AuthEntity master)
           , Key (AuthEntity master) ~ AuthId master
           , PersistStore (YesodPersistBackend master)
           , PersistEntity (AuthEntity master)
           )
        => AuthId master -> HandlerT master IO (Maybe (AuthEntity master))
    getAuthEntity = runDB . get


type family KeyEntity key
type instance KeyEntity (Key x) = x

-- | Similar to 'maybeAuthId', but redirects to a login page if user is not
-- authenticated or responds with error 401 if this is an API client (expecting JSON).
--
-- Since 1.1.0
requireAuthId :: YesodAuth master => HandlerT master IO (AuthId master)
requireAuthId = maybeAuthId >>= maybe handleAuthLack return

-- | Similar to 'maybeAuth', but redirects to a login page if user is not
-- authenticated or responds with error 401 if this is an API client (expecting JSON).
--
-- Since 1.1.0
requireAuth :: ( YesodAuthPersist master
               , val ~ AuthEntity master
               , Key val ~ AuthId master
               , PersistEntity val
               , Typeable val
               ) => HandlerT master IO (Entity val)
requireAuth = maybeAuth >>= maybe handleAuthLack return

-- | Similar to 'requireAuth', but not tied to Persistent's 'Entity' type.
-- Instead, the 'AuthId' and 'AuthEntity' are returned in a tuple.
--
-- Since 1.4.0
requireAuthPair :: (YesodAuthPersist master, Typeable (AuthEntity master))
                => HandlerT master IO (AuthId master, AuthEntity master)
requireAuthPair = maybeAuthPair >>= maybe handleAuthLack return

handleAuthLack :: Yesod master => HandlerT master IO a
handleAuthLack = do
    aj <- acceptsJson
    if aj then notAuthenticated else redirectLogin

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

asHtml :: Html -> Html
asHtml = id
