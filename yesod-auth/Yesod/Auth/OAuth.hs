{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Yesod.Auth.OAuth
    ( authOAuth
    , oauthUrl
    , authTwitter
    , twitterUrl
    ) where

#include "qq.h"

import Yesod.Auth
import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Text.Hamlet (shamlet)
import Web.Authenticate.OAuth
import Data.Maybe
import Data.String
import Data.ByteString.Char8 (pack)
import Control.Arrow ((***))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString (ByteString)
import Control.Applicative ((<$>), (<*>))

oauthUrl :: Text -> AuthRoute
oauthUrl name = PluginR name ["forward"]

authOAuth :: YesodAuth m =>
             Text -- ^ Service Name
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
    oauth = OAuth { oauthServerName = unpack name, oauthRequestUri = reqUrl
                  , oauthAccessTokenUri = accUrl, oauthAuthorizeUri = authUrl
                  , oauthSignatureMethod = HMACSHA1
                  , oauthConsumerKey = fromString key, oauthConsumerSecret = fromString sec
                  , oauthCallback = Nothing
                  }
    dispatch "GET" ["forward"] = do
        render <- getUrlRender
        tm <- getRouteToMaster
        let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
        tok <- liftIO $ getTemporaryCredential oauth'
        redirectText RedirectTemporary (fromString $ authorizeUrl oauth' tok)
    dispatch "GET" [] = do
        (verifier, oaTok) <- runInputGet $ (,)
            <$> ireq textField "oauth_verifier"
            <*> ireq textField "oauth_token"
        let reqTok = Credential [ ("oauth_verifier", encodeUtf8 verifier), ("oauth_token", encodeUtf8 oaTok)
                                ] 
        accTok <- liftIO $ getAccessToken oauth reqTok
        let crId = decodeUtf8With lenientDecode $ fromJust $ lookup (pack ident) $ unCredential accTok
            creds = Creds name crId $ map (bsToText *** bsToText ) $ unCredential accTok
        setCreds True creds
    dispatch _ _ = notFound
    login tm = do
        render <- lift getUrlRender
        let oaUrl = render $ tm $ oauthUrl name
        addHtml
          [QQ(shamlet)| <a href=#{oaUrl}>Login with #{name} |]

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

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
