{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Facebook
    ( authFacebook
    , facebookUrl
    ) where

import Yesod.Auth
import qualified Web.Authenticate.Facebook as Facebook
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)

import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Yesod.Request (languages)
import Text.Hamlet (hamlet)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Control.Monad (mzero)
import Data.Monoid (mappend)
import qualified Data.Aeson.Types
import qualified Yesod.Auth.Message as Msg

facebookUrl :: AuthRoute
facebookUrl = PluginR "facebook" ["forward"]

authFacebook :: YesodAuth m
             => Text -- ^ Application ID
             -> Text -- ^ Application secret
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
        so <- liftIO $ Facebook.getGraphData at "me"
        let c = fromMaybe (error "Invalid response from Facebook")
                $ parseMaybe (parseCreds at') $ either error id so
        setCreds True c
    dispatch _ _ = notFound
    login tm = do
        render <- lift getUrlRender
        let fb = Facebook.Facebook cid secret $ render $ tm url
        let furl = Facebook.getForwardUrl fb $ perms
        y <- lift getYesod
        l <- lift languages
        let mr = renderMessage (getAuth 'x') y l
        addHtml
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
<p>
    <a href="#{furl}">#{mr Msg.Facebook}
|]

parseCreds :: Text -> Value -> Data.Aeson.Types.Parser (Creds m)
parseCreds at' (Object m) = do
    id' <- m .: "id"
    let id'' = "http://graph.facebook.com/" `mappend` id'
    name <- m .: "name"
    email <- m .: "email"
    return
        $ Creds "facebook" id''
        $ maybe id (\x -> (:) ("verifiedEmail", x)) email
        $ maybe id (\x -> (:) ("displayName ", x)) name
        [ ("accessToken", at')
        ]
parseCreds _ _ = mzero
