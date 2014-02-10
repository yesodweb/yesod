{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, QuasiQuotes #-}
module Yesod.Auth.OAuth
    ( authOAuth
    , oauthUrl
    , authTwitter
    , twitterUrl
    , authTumblr
    , tumblrUrl
    , module Web.Authenticate.OAuth
    , OAuthProvider(..)
    , YesodOAuth(..)
    ) where
import           Control.Applicative      ((<$>), (<*>))
import           Control.Arrow            ((***))
import           Control.Exception.Lifted
import           Control.Monad.IO.Class
import           Data.ByteString          (ByteString)
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Typeable
import           Web.Authenticate.OAuth
import           Yesod.Auth
import           Yesod.Form
import           Yesod.Core

data OAuthProvider = Twitter | Tumblr

class (Yesod site, YesodAuth site) => YesodOAuth site where
    getConsumerKey          :: OAuthProvider -> HandlerT site IO ByteString
    getConsumerSecret       :: OAuthProvider -> HandlerT site IO ByteString 
    getAppAccessToken       :: OAuthProvider -> HandlerT site IO ByteString 
    getAppAccessTokenSecret :: OAuthProvider -> HandlerT site IO ByteString 

data YesodOAuthException = CredentialError String Credential
                         | SessionError String
                           deriving (Show, Typeable)

instance Exception YesodOAuthException

oauthUrl :: Text -> AuthRoute
oauthUrl name = PluginR name ["forward"]

authOAuth :: (YesodAuth m, YesodOAuth m)
          => OAuth                        -- ^ 'OAuth' data-type for signing.
          -> (Credential -> IO (Creds m)) -- ^ How to extract ident.
          -> Maybe OAuthProvider
          -> AuthPlugin m
authOAuth oauth mkCreds maybeProvider = AuthPlugin name dispatch login
  where
    name = T.pack $ oauthServerName oauth
    url = PluginR name []
    lookupTokenSecret = bsToText . fromMaybe "" . lookup "oauth_token_secret" . unCredential
    oauthSessionName = "__oauth_token_secret"
    dispatch "GET" ["forward"] = do
        render <- lift getUrlRender
        tm <- getRouteToParent
        let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
        oauth'' <- lift $ addCredentialsIfNecessary oauth'
        master <- lift getYesod
        tok <- lift $ getTemporaryCredential oauth'' (authHttpManager master)
        setSession oauthSessionName $ lookupTokenSecret tok
        redirect $ authorizeUrl oauth'' tok
    dispatch "GET" [] = lift $ do
        Just tokSec <- lookupSession oauthSessionName
        deleteSession oauthSessionName
        reqTok <-
          if oauthVersion oauth == OAuth10
            then do
              oaTok  <- runInputGet $ ireq textField "oauth_token"
              return $ Credential [ ("oauth_token", encodeUtf8 oaTok)
                                  , ("oauth_token_secret", encodeUtf8 tokSec)
                                  ]
            else do
              (verifier, oaTok) <-
                  runInputGet $ (,) <$> ireq textField "oauth_verifier"
                                    <*> ireq textField "oauth_token"
              return $ Credential [ ("oauth_verifier", encodeUtf8 verifier)
                                  , ("oauth_token", encodeUtf8 oaTok)
                                  , ("oauth_token_secret", encodeUtf8 tokSec)
                                  ]
        master <- getYesod
        oauth' <- addCredentialsIfNecessary oauth
        accTok <- getAccessToken oauth' reqTok (authHttpManager master)
        creds  <- liftIO $ mkCreds accTok
        setCreds True creds
    dispatch _ _ = notFound
    addCredentialsIfNecessary oa = case maybeProvider of
        Nothing -> return oa 
        Just provider -> do
            k <- getConsumerKey provider
            s <- getConsumerSecret provider
            return $ oa { oauthConsumerKey     = k
                        , oauthConsumerSecret  = s
                        }
    login tm = do
        render <- getUrlRender
        let oaUrl = render $ tm $ oauthUrl name
        [whamlet| <a href=#{oaUrl}>Login via #{name} |]

mkExtractCreds :: YesodAuth m => Text -> String -> Credential -> IO (Creds m)
mkExtractCreds name idName (Credential dic) = do
  let mcrId = decodeUtf8With lenientDecode <$> lookup (encodeUtf8 $ T.pack idName) dic
  case mcrId of
    Just crId -> return $ Creds name crId $ map (bsToText *** bsToText) dic
    Nothing -> throwIO $ CredentialError ("key not found: " ++ idName) (Credential dic)

authTwitter :: YesodOAuth m
            => AuthPlugin m
authTwitter = authOAuth
                (newOAuth { oauthServerName      = "twitter"
                          , oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
                          , oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
                          , oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize"
                          , oauthSignatureMethod = HMACSHA1
                          , oauthVersion         = OAuth10a
                          })
                (mkExtractCreds "twitter" "screen_name")
                (Just Twitter)

twitterUrl :: AuthRoute
twitterUrl = oauthUrl "twitter"

authTumblr :: YesodOAuth m
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
                Nothing

tumblrUrl :: AuthRoute
tumblrUrl = oauthUrl "tumblr"

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
