{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Facebook
    ( authFacebook
    , facebookLogin
    , facebookUrl
    , facebookLogout
    , getFacebookAccessToken
    ) where

#include "qq.h"

import Yesod.Auth
import qualified Web.Authenticate.Facebook as Facebook
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)

import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Control.Monad (liftM, mzero, when)
import Data.Monoid (mappend)
import qualified Data.Aeson.Types
import qualified Yesod.Auth.Message as Msg

-- | Route for login using this authentication plugin.
facebookLogin :: AuthRoute
facebookLogin = PluginR "facebook" ["forward"]

-- | This is just a synonym of 'facebookLogin'.  Deprecated since
-- @yesod-auth 0.7.8@, please use 'facebookLogin' instead.
facebookUrl :: AuthRoute
facebookUrl = facebookLogin
{-# DEPRECATED facebookUrl "Please use facebookLogin instead." #-}

-- | Route for logout using this authentication plugin.  Per
-- Facebook's policies
-- (<https://developers.facebook.com/policy/>), the user needs to
-- logout from Facebook itself as well.
facebookLogout :: AuthRoute
facebookLogout = PluginR "facebook" ["logout"]

-- | Get Facebook's access token from the session.  Returns
-- @Nothing@ if it's not found (probably because the user is not
-- logged in via Facebook).  Note that the returned access token
-- may have expired.
getFacebookAccessToken :: MonadIO mo => GGHandler sub master mo (Maybe Facebook.AccessToken)
getFacebookAccessToken =
    liftM (fmap Facebook.AccessToken) (lookupSession facebookAccessTokenKey)

-- | Key used to store Facebook's access token in the client
-- session.
facebookAccessTokenKey :: Text
facebookAccessTokenKey = "_FB"

-- | Authentication plugin using Facebook.
authFacebook :: YesodAuth m
             => Text   -- ^ Application ID
             -> Text   -- ^ Application secret
             -> [Text] -- ^ Requested permissions
             -> AuthPlugin m
authFacebook cid secret perms =
    AuthPlugin "facebook" dispatch login
  where
    url = PluginR "facebook" []
    dispatch "GET" ["forward"] = do
        tm <- getRouteToMaster
        render <- getUrlRender
        let fb = Facebook.Facebook cid secret $ render $ tm url
        redirectText RedirectTemporary $ Facebook.getForwardUrl fb perms
    dispatch "GET" [] = do
        render <- getUrlRender
        tm <- getRouteToMaster
        let fb = Facebook.Facebook cid secret $ render $ tm url
        code <- runInputGet $ ireq textField "code"
        at <- liftIO $ Facebook.getAccessToken fb code
        let Facebook.AccessToken at' = at
        setSession facebookAccessTokenKey at'
        so <- liftIO $ Facebook.getGraphData at "me"
        let c = fromMaybe (error "Invalid response from Facebook")
                $ parseMaybe (parseCreds at') $ either error id so
        setCreds True c
    dispatch "GET" ["logout"] = do
        m <- getYesod
        tm <- getRouteToMaster
        mtoken <- getFacebookAccessToken
        when (redirectToReferer m) setUltDestReferer
        case mtoken of
          Nothing -> do
            -- Well... then just logout from our app.
            redirect RedirectTemporary (tm LogoutR)
          Just at -> do
            render <- getUrlRender
            let logout = Facebook.getLogoutUrl at (render $ tm LogoutR)
            redirectText RedirectTemporary logout
    dispatch _ _ = notFound
    login tm = do
        render <- lift getUrlRender
        let fb = Facebook.Facebook cid secret $ render $ tm url
        let furl = Facebook.getForwardUrl fb $ perms
        [QQ(whamlet)|
<p>
    <a href="#{furl}">_{Msg.Facebook}
|]

parseCreds :: Text -> Value -> Data.Aeson.Types.Parser (Creds m)
parseCreds at' (Object m) = do
    id' <- m .: "id"
    let id'' = "http://graph.facebook.com/" `mappend` id'
    name <- m .:? "name"
    email <- m .:? "email"
    return
        $ Creds "facebook" id''
        $ maybe id (\x -> (:) ("verifiedEmail", x)) email
        $ maybe id (\x -> (:) ("displayName ", x)) name
        [ ("accessToken", at')
        ]
parseCreds _ _ = mzero
