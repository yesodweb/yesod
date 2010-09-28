{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Helpers.Auth2
    ( Auth
    , AuthPlugin (..)
    , AuthRoute (..)
    , getAuth
    , Creds (..)
    , YesodAuth (..)
    , setCreds
    , maybeAuthId
    , maybeAuth
    , requireAuthId
    , requireAuth
    , authDummy
    ) where

import Yesod
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Data.ByteString.Char8 as S8
import qualified Network.Wai as W

data Auth = Auth

type Method = String
type Piece = String

data AuthPlugin m = AuthPlugin
    { apName :: String
    , apDispatch :: Method -> [Piece] -> GHandler Auth m ()
    , apLogin :: GWidget Auth m ()
    }

getAuth :: a -> Auth
getAuth = const Auth

-- | User credentials
data Creds m = Creds
    { credsPlugin :: String -- ^ How the user was authenticated
    , credsIdent :: String -- ^ Identifier. Exact meaning depends on plugin.
    , credsExtra :: [(String, String)]
    }

class Yesod m => YesodAuth m where
    type AuthId m

    -- | Default destination on successful login or logout, if no other
    -- destination exists.
    defaultDest :: m -> Route m

    getAuthId :: Creds m -> GHandler s m (Maybe (AuthId m))

    showAuthId :: m -> AuthId m -> String
    readAuthId :: m -> String -> Maybe (AuthId m)

    authPlugins :: [AuthPlugin m]

mkYesodSub "Auth"
    [ ClassP ''YesodAuth [VarT $ mkName "master"]
    ] [$parseRoutes|
/check                 CheckR      GET
/login                 LoginR      GET
/logout                LogoutR     GET POST
/page/#String/*Strings PluginR
|]

credsKey :: String
credsKey = "_ID"

setCreds :: YesodAuth m => Bool -> Creds m -> GHandler s m ()
setCreds doRedirects creds = do
    y <- getYesod
    maid <- getAuthId creds
    case maid of
        Nothing ->
            if doRedirects
                then do
                    case authRoute y of
                        Nothing -> do
                            rh <- defaultLayout $ addBody [$hamlet|
%h1 Invalid login
|]
                            sendResponse rh
                        Just ar -> do
                            setMessage $ string "Invalid login"
                            redirect RedirectTemporary ar
                else return ()
        Just aid -> do
            setSession credsKey $ showAuthId y aid
            if doRedirects
                then do
                    setMessage $ string "You are now logged in"
                    redirect RedirectTemporary $ defaultDest y
                else return ()

getCheckR :: YesodAuth m => GHandler Auth m RepHtmlJson
getCheckR = do
    creds <- maybeAuthId
    defaultLayoutJson (do
        setTitle $ string "Authentication Status"
        addBody $ html creds) (json creds)
  where
    html creds = [$hamlet|
%h1 Authentication Status
$maybe creds _
    %p Logged in.
$nothing
    %p Not logged in.
|]
    json creds =
        jsonMap
            [ ("logged_in", jsonScalar $ maybe "false" (const "true") creds)
            ]

getLoginR :: YesodAuth m => GHandler Auth m RepHtml
getLoginR = defaultLayout $ do
    setTitle $ string "Login"
    mapM_ apLogin authPlugins

getLogoutR :: YesodAuth m => GHandler Auth m ()
getLogoutR = postLogoutR -- FIXME redirect to post

postLogoutR :: YesodAuth m => GHandler Auth m ()
postLogoutR = do
    y <- getYesod
    deleteSession credsKey
    redirectUltDest RedirectTemporary $ defaultDest y

handlePluginR :: YesodAuth m => String -> [String] -> GHandler Auth m ()
handlePluginR plugin pieces = do
    env <- waiRequest
    let method = S8.unpack $ W.requestMethod env
    case filter (\x -> apName x == plugin) authPlugins of
        [] -> notFound
        ap:_ -> apDispatch ap method pieces

-- | Retrieves user credentials, if user is authenticated.
maybeAuthId :: YesodAuth m => GHandler s m (Maybe (AuthId m))
maybeAuthId = do
    ms <- lookupSession credsKey
    y <- getYesod
    case ms of
        Nothing -> return Nothing
        Just s -> return $ readAuthId y s

maybeAuth :: ( YesodAuth m
             , Key val ~ AuthId m
             , PersistBackend (YesodDB m (GHandler s m))
             , PersistEntity val
             , YesodPersist m
             ) => GHandler s m (Maybe (Key val, val))
maybeAuth = do
    maid <- maybeAuthId
    case maid of
        Nothing -> return Nothing
        Just aid -> do
            ma <- runDB $ get aid
            case ma of
                Nothing -> return Nothing
                Just a -> return $ Just (aid, a)

requireAuthId :: YesodAuth m => GHandler s m (AuthId m)
requireAuthId = maybeAuthId >>= maybe redirectLogin return

requireAuth :: ( YesodAuth m
               , Key val ~ AuthId m
               , PersistBackend (YesodDB m (GHandler s m))
               , PersistEntity val
               , YesodPersist m
               ) => GHandler s m (Key val, val)
requireAuth = maybeAuth >>= maybe redirectLogin return

redirectLogin :: Yesod m => GHandler s m a
redirectLogin = do
    y <- getYesod
    setUltDest'
    case authRoute y of
        Just z -> redirect RedirectTemporary z
        Nothing -> permissionDenied "Please configure authRoute"

authDummy :: YesodAuth m => AuthPlugin m
authDummy =
    AuthPlugin "dummy" dispatch login
  where
    dispatch "POST" [] = do
        ident <- runFormPost' $ stringInput "ident"
        setCreds True $ Creds "dummy" ident []
    dispatch _ _ = notFound
    url = PluginR "dummy" []
    authToMaster = liftHandler getRouteToMaster
    login = do
        tm <- authToMaster
        addBody [$hamlet|
%form!method=post!action=@tm.url@
    Your new identifier is: $
    %input!type=text!name=ident
    %input!type=submit!value="Dummy Login"
|]
