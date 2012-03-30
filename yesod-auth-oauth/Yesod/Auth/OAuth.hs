{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Yesod.Auth.OAuth
    ( authOAuth
    , oauthUrl
    , authTwitter
    , twitterUrl
	, authTumblr
	, tumblrUrl
    , module Web.Authenticate.OAuth
    ) where

import Yesod.Auth
import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Web.Authenticate.OAuth
import Data.Maybe
import Control.Arrow ((***))
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString (ByteString)
import Control.Applicative ((<$>), (<*>))

oauthUrl :: Text -> AuthRoute
oauthUrl name = PluginR name ["forward"]

authOAuth :: YesodAuth m
          => OAuth                        -- ^ 'OAuth' data-type for signing.
          -> (Credential -> IO (Creds m)) -- ^ How to extract ident. 
          -> AuthPlugin m
authOAuth oauth mkCreds = AuthPlugin name dispatch login
  where
    name = T.pack $ oauthServerName oauth
    url = PluginR name []
    lookupTokenSecret = bsToText . fromMaybe "" . lookup "oauth_token_secret" . unCredential
    oauthSessionName = "__oauth_token_secret"
    dispatch "GET" ["forward"] = do
        render <- getUrlRender
        tm <- getRouteToMaster
        let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
        master <- getYesod
        tok <- lift $ getTemporaryCredential oauth' (authHttpManager master)
        setSession oauthSessionName $ lookupTokenSecret tok
        redirect $ authorizeUrl oauth' tok
    dispatch "GET" [] = do
      reqTok <-
        if oauthVersion oauth == OAuth10
          then do
            oaTok  <- runInputGet $ ireq textField "oauth_token"
            tokSec <- fromJust <$> lookupSession oauthSessionName
            deleteSession oauthSessionName
            return $ Credential [ ("oauth_token", encodeUtf8 oaTok)
                                , ("oauth_token_secret", encodeUtf8 tokSec)
                                ]
          else do
            (verifier, oaTok) <-
                runInputGet $ (,) <$> ireq textField "oauth_verifier"
                                  <*> ireq textField "oauth_token"
            tokSec <- fromJust <$> lookupSession oauthSessionName
            deleteSession oauthSessionName
            return $ Credential [ ("oauth_verifier", encodeUtf8 verifier)
                                , ("oauth_token", encodeUtf8 oaTok)
                                , ("oauth_token_secret", encodeUtf8 tokSec)
                                ]
      master <- getYesod
      accTok <- lift $ getAccessToken oauth reqTok (authHttpManager master)
      creds  <- liftIO $ mkCreds accTok
      setCreds True creds
    dispatch _ _ = notFound
    login tm = do
        render <- lift getUrlRender
        let oaUrl = render $ tm $ oauthUrl name
        addWidget
          [whamlet| <a href=#{oaUrl}>Login via #{name} |]

authTwitter :: YesodAuth m
            => ByteString -- ^ Consumer Key
            -> ByteString -- ^ Consumer Secret
            -> AuthPlugin m
authTwitter key secret = authOAuth
                (newOAuth { oauthServerName      = "twitter"
                          , oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
                          , oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
                          , oauthAuthorizeUri    =  "https://api.twitter.com/oauth/authorize"
                          , oauthSignatureMethod = HMACSHA1
                          , oauthConsumerKey     = key
                          , oauthConsumerSecret  = secret
                          , oauthVersion         = OAuth10a
                          })
                extractCreds
  where
    extractCreds (Credential dic) = do
        let crId = decodeUtf8With lenientDecode $ fromJust $ lookup "screen_name" dic
        return $ Creds "twitter" crId $ map (bsToText *** bsToText ) dic

twitterUrl :: AuthRoute
twitterUrl = oauthUrl "twitter"

authTumblr :: YesodAuth m
            => ByteString -- ^ Consumer Key
            -> ByteString -- ^ Consumer Secret
            -> AuthPlugin m
authTumblr key secret = authOAuth
                (newOAuth { oauthServerName      = "tumblr"
                          , oauthRequestUri      = "http://www.tumblr.com/oauth/request_token"
                          , oauthAccessTokenUri  = "http://www.tumblr.com/oauth/access_token"
                          , oauthAuthorizeUri    = "http://www.tumblr.com/oauth/authorize"
                          , oauthSignatureMethod = HMACSHA1
                          , oauthConsumerKey     = key
                          , oauthConsumerSecret  = secret
                          , oauthVersion         = OAuth10a
                          })
                extractCreds
  where
    extractCreds (Credential dic) = do
        let crId = decodeUtf8With lenientDecode $ fromJust $ lookup "name" dic
        return $ Creds "tumblr" crId $ map (bsToText *** bsToText ) dic

tumblrUrl :: AuthRoute
tumblrUrl = oauthUrl "tumblr"

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
