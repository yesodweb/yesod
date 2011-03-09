{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Yesod.Helpers.Auth.OAuth
    ( authOAuth
    , oauthUrl
    , authTwitter
    , twitterUrl
    ) where
import Yesod.Helpers.Auth
import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Text.Hamlet (hamlet)
import Web.Authenticate.OAuth
import Data.Maybe
import Data.String
import Network.HTTP.Enumerator
import Data.ByteString.Char8 (unpack, pack)
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

oauthUrl :: String -> AuthRoute
oauthUrl name = PluginR name ["forward"]

authOAuth :: YesodAuth m =>
             String -- ^ Service Name
          -> String -- ^ OAuth Parameter Name to use for identify
          -> String -- ^ Request URL
          -> String -- ^ Access Token URL
          -> String -- ^ Authorize URL
          -> String -- ^ Consumer Key
          -> String -- ^ Consumer Secret
          -> AuthPlugin m
authOAuth name ident reqUrl accUrl authUrl key sec = AuthPlugin name dispatch login
  where
    url = PluginR name []
    oauth = OAuth { oauthServerName = name, oauthRequestUri = reqUrl
                  , oauthAccessTokenUri = accUrl, oauthAuthorizeUri = authUrl
                  , oauthSignatureMethod = HMACSHA1
                  , oauthConsumerKey = fromString key, oauthConsumerSecret = fromString sec
                  , oauthCallback = Nothing
                  }
    dispatch "GET" ["forward"] = do
        render <- getUrlRender
        tm <- getRouteToMaster
        let oauth' = oauth { oauthCallback = Just $ fromString $ render $ tm url }
        tok <- liftIO $ getTemporaryCredential oauth'
        redirectString RedirectTemporary (fromString $ authorizeUrl oauth' tok)
    dispatch "GET" [] = do
        render <- getUrlRender
        tm <- getRouteToMaster
        let callback = render $ tm url
        verifier <- runFormGet' $ stringInput "oauth_verifier"
        oaTok    <- runFormGet' $ stringInput "oauth_token"
        let reqTok = Credential [ ("oauth_verifier", pack verifier), ("oauth_token", pack oaTok)
                                ] 
        accTok <- liftIO $ getAccessToken oauth reqTok
        let crId = unpack $ fromJust $ lookup (pack ident) $ unCredential accTok
            creds = Creds name crId $ map (unpack *** unpack) $ unCredential accTok
        setCreds True creds
    dispatch _ _ = notFound
    login tm = do
        render <- lift getUrlRender
        let oaUrl = render $ tm $ oauthUrl name
        addHtml
#if GHC7
          [hamlet|
#else
          [$hamlet|
#endif
             <a href=#{oaUrl}>Login with #{name}
          |]

authTwitter :: YesodAuth m =>
               String -- ^ Consumer Key
            -> String -- ^ Consumer Secret
            -> AuthPlugin m
authTwitter = authOAuth "twitter"
                        "screen_name"
                        "http://twitter.com/oauth/request_token"
                        "http://twitter.com/oauth/access_token"
                        "http://twitter.com/oauth/authorize"

twitterUrl :: AuthRoute
twitterUrl = oauthUrl "twitter"
