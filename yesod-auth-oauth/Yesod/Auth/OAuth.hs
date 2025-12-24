{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yesod.Auth.OAuth
    ( authOAuth
    , oauthUrl
    , authTwitter
    , authTwitterUsingUserId
    , twitterUrl
    , authTumblr
    , tumblrUrl
    , module Web.Authenticate.OAuth
    ) where
import           Control.Arrow            ((***))
import           UnliftIO.Exception
import           Control.Monad.IO.Class
import           Data.ByteString          (ByteString)
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)
import           Web.Authenticate.OAuth
import           Yesod.Auth
import           Yesod.Form
import           Yesod.Core

data YesodOAuthException = CredentialError String Credential
                         | SessionError String
                           deriving Show

instance Exception YesodOAuthException

oauthUrl :: Text -> AuthRoute
oauthUrl name = PluginR name ["forward"]

authOAuth :: forall master. YesodAuth master
          => OAuth                        -- ^ 'OAuth' data-type for signing.
          -> (Credential -> IO (Creds master)) -- ^ How to extract ident.
          -> AuthPlugin master
authOAuth oauth mkCreds = AuthPlugin name dispatch login
  where
    name = T.pack $ oauthServerName oauth
    url = PluginR name []
    lookupTokenSecret = bsToText . fromMaybe "" . lookup "oauth_token_secret" . unCredential

    oauthSessionName :: Text
    oauthSessionName = "__oauth_token_secret"

    dispatch
      :: Text
      -> [Text]
      -> AuthHandler master TypedContent
    dispatch "GET" ["forward"] = do
        render <- getUrlRender
        tm <- getRouteToParent
        let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
        manager <- authHttpManager
        tok <- getTemporaryCredential oauth' manager
        setSession oauthSessionName $ lookupTokenSecret tok
        redirect $ authorizeUrl oauth' tok
    dispatch "GET" [] = do
      tokSec <- lookupSession oauthSessionName >>= \case
        Just t -> return t
        Nothing -> liftIO $ fail "lookupSession could not find session"
      deleteSession oauthSessionName
      reqTok <-
        if oauthVersion oauth == OAuth10
          then do
            oaTok  <- liftHandler $ runInputGet $ ireq textField "oauth_token"
            return $ Credential [ ("oauth_token", encodeUtf8 oaTok)
                                , ("oauth_token_secret", encodeUtf8 tokSec)
                                ]
          else do
            (verifier, oaTok) <- liftHandler $
                runInputGet $ (,) <$> ireq textField "oauth_verifier"
                                  <*> ireq textField "oauth_token"
            return $ Credential [ ("oauth_verifier", encodeUtf8 verifier)
                                , ("oauth_token", encodeUtf8 oaTok)
                                , ("oauth_token_secret", encodeUtf8 tokSec)
                                ]
      manager <- authHttpManager
      accTok <- getAccessToken oauth reqTok manager
      creds  <- liftIO $ mkCreds accTok
      setCredsRedirect creds
    dispatch _ _ = notFound

    login tm = do
        render <- getUrlRender
        let oaUrl = render $ tm $ oauthUrl name
        [whamlet| <a href=#{oaUrl}>Login via #{name} |]

mkExtractCreds :: Text -> String -> Credential -> IO (Creds m)
mkExtractCreds name idName (Credential dic) = do
  let mcrId = decodeUtf8With lenientDecode <$> lookup (encodeUtf8 $ T.pack idName) dic
  case mcrId of
    Just crId -> return $ Creds name crId $ map (bsToText *** bsToText) dic
    Nothing -> throwIO $ CredentialError ("key not found: " ++ idName) (Credential dic)

authTwitter' :: YesodAuth m
             => ByteString -- ^ Consumer Key
             -> ByteString -- ^ Consumer Secret
             -> String
             -> AuthPlugin m
authTwitter' key secret idName = authOAuth
                (newOAuth { oauthServerName      = "twitter"
                          , oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
                          , oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
                          , oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize"
                          , oauthSignatureMethod = HMACSHA1
                          , oauthConsumerKey     = key
                          , oauthConsumerSecret  = secret
                          , oauthVersion         = OAuth10a
                          })
                (mkExtractCreds "twitter" idName)

-- | This plugin uses Twitter's /screen_name/ as ID, which shouldn't be used for authentication because it is mutable.
authTwitter :: YesodAuth m
            => ByteString -- ^ Consumer Key
            -> ByteString -- ^ Consumer Secret
            -> AuthPlugin m
authTwitter key secret = authTwitter' key secret "screen_name"
{-# DEPRECATED authTwitter "Use authTwitterUsingUserId instead" #-}

-- | Twitter plugin which uses Twitter's /user_id/ as ID.
--
-- For more information, see: https://github.com/yesodweb/yesod/pull/1168
--
-- @since 1.4.1
authTwitterUsingUserId :: YesodAuth m
                  => ByteString -- ^ Consumer Key
                  -> ByteString -- ^ Consumer Secret
                  -> AuthPlugin m
authTwitterUsingUserId key secret = authTwitter' key secret "user_id"

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
                (mkExtractCreds "tumblr" "name")

tumblrUrl :: AuthRoute
tumblrUrl = oauthUrl "tumblr"

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
