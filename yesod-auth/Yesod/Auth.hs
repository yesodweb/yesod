{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Auth
    ( -- * Subsite
      Auth
    , AuthRoute
    , AuthHandler
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
    , AuthenticationResult (..)
    , defaultMaybeAuthId
    , defaultLoginHandler
    , maybeAuthPair
    , maybeAuth
    , requireAuthId
    , requireAuthPair
    , requireAuth
      -- * Exception
    , AuthException (..)
      -- * Internal
    , credsKey
    , provideJsonMessage
    , messageJson401
    , asHtml
    ) where

import RIO
import Control.Monad.Trans.Maybe

import Yesod.Auth.Routes
import Data.Aeson hiding (json)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as Map
import Data.Monoid (Endo)
import Network.HTTP.Client (Manager, Request, withResponse, Response, BodyReader)
import Network.HTTP.Client.TLS (getGlobalManager)

import qualified Network.Wai as W

import Yesod.Core
import Yesod.Persist
import Yesod.Auth.Message (AuthMessage, defaultMessage)
import qualified Yesod.Auth.Message as Msg
import Yesod.Form (FormMessage)
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Network.HTTP.Types (Status, internalServerError500, unauthorized401)
import qualified Control.Monad.Trans.Writer    as Writer
import Control.Monad (void)

type AuthHandler site = SubHandlerFor Auth site

type AuthRoute = Route Auth

type Method = Text
type Piece = Text

-- | The result of an authentication based on credentials
--
-- @since 1.4.4
data AuthenticationResult master
    = Authenticated (AuthId master) -- ^ Authenticated successfully
    | UserError AuthMessage         -- ^ Invalid credentials provided by user
    | ServerError Text              -- ^ Some other error

data AuthPlugin master = AuthPlugin
    { apName :: Text
    , apDispatch :: Method -> [Piece] -> SubHandlerFor Auth master TypedContent
    , apLogin :: (Route Auth -> Route master) -> WidgetFor master ()
    }

getAuth :: a -> Auth
getAuth = const Auth

-- | User credentials
data Creds master = Creds
    { credsPlugin :: Text -- ^ How the user was authenticated
    , credsIdent :: Text -- ^ Identifier. Exact meaning depends on plugin.
    , credsExtra :: [(Text, Text)]
    } deriving (Show)

class (Yesod master, PathPiece (AuthId master), RenderMessage master FormMessage) => YesodAuth master where
    type AuthId master

    -- | specify the layout. Uses defaultLayout by default
    authLayout :: (HasHandlerData env, HandlerSite env ~ master) => WidgetFor master () -> RIO env Html
    authLayout = liftHandler . defaultLayout

    -- | Default destination on successful login, if no other
    -- destination exists.
    loginDest :: master -> Route master

    -- | Default destination on successful logout, if no other
    -- destination exists.
    logoutDest :: master -> Route master

    -- | Perform authentication based on the given credentials.
    --
    -- Default implementation is in terms of @'getAuthId'@
    --
    -- @since: 1.4.4
    authenticate :: (HasHandlerData env, HandlerSite env ~ master) => Creds master -> RIO env (AuthenticationResult master)
    authenticate creds = do
        muid <- getAuthId creds

        return $ maybe (UserError Msg.InvalidLogin) Authenticated muid

    -- | Determine the ID associated with the set of credentials.
    --
    -- Default implementation is in terms of @'authenticate'@
    --
    getAuthId :: (HasHandlerData env, HandlerSite env ~ master) => Creds master -> RIO env (Maybe (AuthId master))
    getAuthId creds = do
        auth <- authenticate creds

        return $ case auth of
            Authenticated auid -> Just auid
            _ -> Nothing

    -- | Which authentication backends to use.
    authPlugins :: master -> [AuthPlugin master]

    -- | What to show on the login page.
    --
    -- By default this calls 'defaultLoginHandler', which concatenates
    -- plugin widgets and wraps the result in 'authLayout'. Override if
    -- you need fancy widget containers, additional functionality, or an
    -- entirely custom page.  For example, in some applications you may
    -- want to prevent the login page being displayed for a user who is
    -- already logged in, even if the URL is visited explicitly; this can
    -- be done by overriding 'loginHandler' in your instance declaration
    -- with something like:
    --
    -- > instance YesodAuth App where
    -- >     ...
    -- >     loginHandler = do
    -- >         ma <- lift maybeAuthId
    -- >         when (isJust ma) $
    -- >             lift $ redirect HomeR   -- or any other Handler code you want
    -- >         defaultLoginHandler
    --
    loginHandler
      :: (HasHandlerData env, SubHandlerSite env ~ Auth, HandlerSite env ~ master)
      => RIO env Html
    loginHandler = defaultLoginHandler

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

    -- | When being redirected to the login page should the current page
    -- be set to redirect back to. Default is 'True'.
    --
    -- @since 1.4.21
    redirectToCurrent :: master -> Bool
    redirectToCurrent _ = True

    -- | Return an HTTP connection manager that is stored in the foundation
    -- type. This allows backends to reuse persistent connections. If none of
    -- the backends you're using use HTTP connections, you can safely return
    -- @error \"authHttpManager\"@ here.
    authHttpManager :: (HasHandlerData env, HandlerSite env ~ master) => RIO env Manager
    authHttpManager = liftIO getGlobalManager

    -- | Called on a successful login. By default, calls
    -- @addMessageI "success" NowLoggedIn@.
    onLogin :: (HasHandlerData env, master ~ HandlerSite env) => RIO env ()
    onLogin = addMessageI "success" Msg.NowLoggedIn

    -- | Called on logout. By default, does nothing
    onLogout :: (HasHandlerData env, master ~ HandlerSite env) => RIO env ()
    onLogout = return ()

    -- | Retrieves user credentials, if user is authenticated.
    --
    -- By default, this calls 'defaultMaybeAuthId' to get the user ID from the
    -- session. This can be overridden to allow authentication via other means,
    -- such as checking for a special token in a request header. This is
    -- especially useful for creating an API to be accessed via some means
    -- other than a browser.
    --
    -- @since 1.2.0
    maybeAuthId :: (HasHandlerData env, master ~ HandlerSite env) => RIO env (Maybe (AuthId master))

    default maybeAuthId
        :: (HasHandlerData env, master ~ HandlerSite env, YesodAuthPersist master, Typeable (AuthEntity master))
        => RIO env (Maybe (AuthId master))
    maybeAuthId = defaultMaybeAuthId

    -- | Called on login error for HTTP requests. By default, calls
    -- @addMessage@ with "error" as status and redirects to @dest@.
    onErrorHtml
      :: (HasHandlerData env, HandlerSite env ~ master)
      => Route master
      -> Text
      -> RIO env Html
    onErrorHtml dest msg = do
        addMessage "error" $ toHtml msg
        fmap asHtml $ redirect dest

    -- | runHttpRequest gives you a chance to handle an HttpException and retry
    --  The default behavior is to simply execute the request which will throw an exception on failure
    --
    --  The HTTP 'Request' is given in case it is useful to change behavior based on inspecting the request.
    --  This is an experimental API that is not broadly used throughout the yesod-auth code base
    runHttpRequest
      :: (HasHandlerData env, HandlerSite env ~ master)
      => Request
      -> (Response BodyReader -> RIO env a)
      -> RIO env a
    runHttpRequest req inner = do
      man <- authHttpManager
      withRunInIO $ \run -> withResponse req man $ run . inner

    {-# MINIMAL loginDest, logoutDest, (authenticate | getAuthId), authPlugins #-}

{-# DEPRECATED getAuthId "Define 'authenticate' instead; 'getAuthId' will be removed in the next major version" #-}

-- | Internal session key used to hold the authentication information.
--
-- @since 1.2.3
credsKey :: Text
credsKey = "_ID"

-- | Retrieves user credentials from the session, if user is authenticated.
--
-- This function does /not/ confirm that the credentials are valid, see
-- 'maybeAuthIdRaw' for more information. The first call in a request
-- does a database request to make sure that the account is still in the database.
--
-- @since 1.1.2
defaultMaybeAuthId
    :: (HasHandlerData env, HandlerSite env ~ master, YesodAuthPersist master, Typeable (AuthEntity master))
    => RIO env (Maybe (AuthId master))
defaultMaybeAuthId = runMaybeT $ do
    s   <- MaybeT $ lookupSession credsKey
    aid <- MaybeT $ return $ fromPathPiece s
    _   <- MaybeT $ cachedAuth aid
    return aid

cachedAuth
    :: ( HasHandlerData env
       , YesodAuthPersist master
       , Typeable (AuthEntity master)
       , HandlerSite env ~ master
       )
    => AuthId master
    -> RIO env (Maybe (AuthEntity master))
cachedAuth
    = fmap unCachedMaybeAuth
    . cached
    . fmap CachedMaybeAuth
    . getAuthEntity


-- | Default handler to show the login page.
--
-- This is the default 'loginHandler'.  It concatenates plugin widgets and
-- wraps the result in 'authLayout'.  See 'loginHandler' for more details.
--
-- @since 1.4.9
defaultLoginHandler
  :: (HasHandlerData env, SubHandlerSite env ~ Auth, YesodAuth (HandlerSite env))
  => RIO env Html
defaultLoginHandler = do
    tp <- getRouteToParent
    authLayout $ do
        setTitleI Msg.LoginTitle
        master <- getYesod
        mapM_ (flip apLogin tp) (authPlugins master)

loginErrorMessageI
  :: (HasHandlerData env, SubHandlerSite env ~ Auth, YesodAuth (HandlerSite env))
  => Route Auth
  -> AuthMessage
  -> RIO env TypedContent
loginErrorMessageI dest msg = do
  toParent <- getRouteToParent
  loginErrorMessageMasterI (toParent dest) msg


loginErrorMessageMasterI
  :: (HasHandlerData env, HandlerSite env ~ master, YesodAuth master)
  => Route master
  -> AuthMessage
  -> RIO env TypedContent
loginErrorMessageMasterI dest msg = do
  mr <- getMessageRender
  loginErrorMessage dest (mr msg)

-- | For HTML, set the message and redirect to the route.
-- For JSON, send the message and a 401 status
loginErrorMessage
         :: (HasHandlerData env, YesodAuth (HandlerSite env))
         => Route (HandlerSite env)
         -> Text
         -> RIO env TypedContent
loginErrorMessage dest msg = messageJson401 msg (onErrorHtml dest msg)

messageJson401
  :: HasHandlerData env
  => Text
  -> RIO env Html
  -> RIO env TypedContent
messageJson401 = messageJsonStatus unauthorized401

messageJson500 :: HasHandlerData env => Text -> RIO env Html -> RIO env TypedContent
messageJson500 = messageJsonStatus internalServerError500

messageJsonStatus
  :: HasHandlerData env
  => Status
  -> Text
  -> RIO env Html
  -> RIO env TypedContent
messageJsonStatus status msg html = selectRep $ do
    provideRep html
    provideRep $ do
        let obj = object ["message" .= msg]
        void $ sendResponseStatus status obj
        return obj

provideJsonMessage :: Monad m => Text -> Writer.Writer (Endo [ProvidedRep m]) ()
provideJsonMessage msg = provideRep $ return $ object ["message" .= msg]


setCredsRedirect
  :: (HasHandlerData env, YesodAuth (HandlerSite env))
  => Creds (HandlerSite env) -- ^ new credentials
  -> RIO env TypedContent
setCredsRedirect creds = do
    y    <- getYesod
    auth <- authenticate creds
    case auth of
        Authenticated aid -> do
            setSession credsKey $ toPathPiece aid
            onLogin
            res <- selectRep $ do
                provideRepType typeHtml $
                    fmap asHtml $ redirectUltDest $ loginDest y
                provideJsonMessage "Login Successful"
            sendResponse res

        UserError msg ->
            case authRoute y of
                Nothing -> do
                    msg' <- renderMessage' msg
                    messageJson401 msg' $ authLayout $ -- TODO
                        toWidget [whamlet|<h1>_{msg}|]
                Just ar -> loginErrorMessageMasterI ar msg

        ServerError msg -> do
            logError $ display msg

            case authRoute y of
                Nothing -> do
                    msg' <- renderMessage' Msg.AuthError
                    messageJson500 msg' $ authLayout $
                        toWidget [whamlet|<h1>_{Msg.AuthError}|]
                Just ar -> loginErrorMessageMasterI ar Msg.AuthError

  where
    renderMessage' msg = do
        langs <- languages
        master <- getYesod
        return $ renderAuthMessage master langs msg

-- | Sets user credentials for the session after checking them with authentication backends.
setCreds :: (HasHandlerData env, YesodAuth (HandlerSite env))
         => Bool                  -- ^ if HTTP redirects should be done
         -> Creds (HandlerSite env) -- ^ new credentials
         -> RIO env ()
setCreds doRedirects creds =
    if doRedirects
      then void $ setCredsRedirect creds
      else do auth <- authenticate creds
              case auth of
                  Authenticated aid -> setSession credsKey $ toPathPiece aid
                  _ -> return ()

-- | same as defaultLayoutJson, but uses authLayout
authLayoutJson
  :: (ToJSON j, HasHandlerData env, YesodAuth (HandlerSite env))
  => WidgetFor (HandlerSite env) ()  -- ^ HTML
  -> RIO env j  -- ^ JSON
  -> RIO env TypedContent
authLayoutJson w json = selectRep $ do
    provideRep $ authLayout w
    provideRep $ fmap toJSON json

-- | Clears current user credentials for the session.
--
-- @since 1.1.7
clearCreds :: (HasHandlerData env, YesodAuth (HandlerSite env))
           => Bool -- ^ if HTTP redirect to 'logoutDest' should be done
           -> RIO env ()
clearCreds doRedirects = do
    y <- getYesod
    onLogout
    deleteSession credsKey
    when doRedirects $ do
        redirectUltDest $ logoutDest y

getCheckR :: (YesodAuth (HandlerSite env), HasHandlerData env) => RIO env TypedContent
getCheckR = do
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

setUltDestReferer' :: (HasHandlerData env, YesodAuth (HandlerSite env)) => RIO env ()
setUltDestReferer' = do
    master <- getYesod
    when (redirectToReferer master) setUltDestReferer

getLoginR :: (HasHandlerData env, YesodAuth (HandlerSite env), SubHandlerSite env ~ Auth) => RIO env Html
getLoginR = setUltDestReferer' >> loginHandler

getLogoutR :: (HasHandlerData env, YesodAuth (HandlerSite env), SubHandlerSite env ~ Auth) => RIO env ()
getLogoutR = do
  tp <- getRouteToParent
  setUltDestReferer' >> redirectToPost (tp LogoutR)

postLogoutR :: (HasHandlerData env, YesodAuth (HandlerSite env)) => RIO env ()
postLogoutR = clearCreds True

handlePluginR
  :: YesodAuth site
  => Text
  -> [Text]
  -> SubHandlerFor Auth site TypedContent
handlePluginR plugin pieces = do
    master <- getYesod
    env <- waiRequest
    let method = decodeUtf8With lenientDecode $ W.requestMethod env
    case filter (\x -> apName x == plugin) (authPlugins master) of
        [] -> notFound
        ap:_ -> apDispatch ap method pieces

-- | Similar to 'maybeAuthId', but additionally look up the value associated
-- with the user\'s database identifier to get the value in the database. This
-- assumes that you are using a Persistent database.
--
-- @since 1.1.0
maybeAuth :: ( YesodAuthPersist master
             , val ~ AuthEntity master
             , Key val ~ AuthId master
             , PersistEntity val
             , Typeable val
             , HasHandlerData env
             , HandlerSite env ~ master
             ) => RIO env (Maybe (Entity val))
maybeAuth = fmap (fmap (uncurry Entity)) maybeAuthPair

-- | Similar to 'maybeAuth', but doesn’t assume that you are using a
-- Persistent database.
--
-- @since 1.4.0
maybeAuthPair
  :: ( YesodAuthPersist master
     , Typeable (AuthEntity master)
     , HasHandlerData env
     , HandlerSite env ~ master
     )
  => RIO env (Maybe (AuthId master, AuthEntity master))
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
-- @since 1.4.0
class (YesodAuth master, YesodPersist master) => YesodAuthPersist master where
    -- | If the @AuthId@ for a given site is a persistent ID, this will give the
    -- value for that entity. E.g.:
    --
    -- > type AuthId MySite = UserId
    -- > AuthEntity MySite ~ User
    --
    -- @since 1.2.0
    type AuthEntity master :: *
    type AuthEntity master = KeyEntity (AuthId master)

    getAuthEntity
      :: (HasHandlerData env, HandlerSite env ~ master)
      => AuthId master
      -> RIO env (Maybe (AuthEntity master))

    default getAuthEntity
        :: ( YesodPersistBackend master ~ backend
           , PersistRecordBackend (AuthEntity master) backend
           , Key (AuthEntity master) ~ AuthId master
           , PersistStore backend
           , HasHandlerData env
           , HandlerSite env ~ master
           )
        => AuthId master
        -> RIO env (Maybe (AuthEntity master))
    getAuthEntity = liftHandler . runDB . get


type family KeyEntity key
type instance KeyEntity (Key x) = x

-- | Similar to 'maybeAuthId', but redirects to a login page if user is not
-- authenticated or responds with error 401 if this is an API client (expecting JSON).
--
-- @since 1.1.0
requireAuthId :: (HasHandlerData env, YesodAuth (HandlerSite env)) => RIO env (AuthId (HandlerSite env))
requireAuthId = maybeAuthId >>= maybe handleAuthLack return

-- | Similar to 'maybeAuth', but redirects to a login page if user is not
-- authenticated or responds with error 401 if this is an API client (expecting JSON).
--
-- @since 1.1.0
requireAuth :: ( YesodAuthPersist master
               , val ~ AuthEntity master
               , Key val ~ AuthId master
               , PersistEntity val
               , Typeable val
               , HasHandlerData env
               , HandlerSite env ~ master
               ) => RIO env (Entity val)
requireAuth = maybeAuth >>= maybe handleAuthLack return

-- | Similar to 'requireAuth', but not tied to Persistent's 'Entity' type.
-- Instead, the 'AuthId' and 'AuthEntity' are returned in a tuple.
--
-- @since 1.4.0
requireAuthPair
  :: ( YesodAuthPersist master
     , Typeable (AuthEntity master)
     , HasHandlerData env
     , HandlerSite env ~ master
     )
  => RIO env (AuthId master, AuthEntity master)
requireAuthPair = maybeAuthPair >>= maybe handleAuthLack return

handleAuthLack :: (YesodAuth (HandlerSite env), HasHandlerData env) => RIO env a
handleAuthLack = do
    aj <- acceptsJson
    if aj then notAuthenticated else redirectLogin

redirectLogin :: (YesodAuth (HandlerSite env), HasHandlerData env) => RIO env a
redirectLogin = do
    y <- getYesod
    when (redirectToCurrent y) setUltDestCurrent
    case authRoute y of
        Just z -> redirect z
        Nothing -> permissionDenied "Please configure authRoute"

instance YesodAuth master => RenderMessage master AuthMessage where
    renderMessage = renderAuthMessage

data AuthException = InvalidFacebookResponse
    deriving (Show, Typeable)
instance Exception AuthException

instance YesodAuth master => YesodSubDispatch Auth master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuth)

asHtml :: Html -> Html
asHtml = id
