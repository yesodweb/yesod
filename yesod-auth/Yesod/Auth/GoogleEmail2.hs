{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- | Use an email address as an identifier via Google's login system.
--
-- Note that this is a replacement for "Yesod.Auth.GoogleEmail", which depends
-- on Google's now deprecated OpenID system. For more information, see
-- <https://developers.google.com/+/api/auth-migration>.
--
-- By using this plugin, you are trusting Google to validate an email address,
-- and requiring users to have a Google account. On the plus side, you get to
-- use email addresses as the identifier, many users have existing Google
-- accounts, the login system has been long tested (as opposed to BrowserID),
-- and it requires no credential managing or setup (as opposed to Email).
--
-- In order to use this plugin:
--
-- * Create an application on the Google Developer Console <https://console.developers.google.com/>
--
-- * Create OAuth credentials. The redirect URI will be <http://yourdomain/auth/page/googleemail2/complete>. (If you have your authentication subsite at a different root than \/auth\/, please adjust accordingly.)
--
-- * Enable the Google+ API.
--
-- Since 1.3.1
module Yesod.Auth.GoogleEmail2
    ( authGoogleEmail
    , forwardUrl
    ) where

import           Blaze.ByteString.Builder (fromByteString, toByteString)
import           Control.Applicative      ((<$>), (<*>))
import           Control.Arrow            (second)
import           Control.Monad            (liftM, unless)
import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode        as A
import           Data.Aeson.Parser        (json')
import           Data.Aeson.Types         (FromJSON (parseJSON), parseEither,
                                           withObject)
import           Data.Conduit             (($$+-))
import           Data.Conduit.Attoparsec  (sinkParser)
import qualified Data.HashMap.Strict      as M
import           Data.Monoid              (mappend)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TL
import           Network.HTTP.Client      (parseUrl, requestHeaders,
                                           responseBody, urlEncodedBody)
import           Network.HTTP.Conduit     (http)
import           Network.HTTP.Types       (renderQueryText)
import           Network.Mail.Mime        (randomString)
import           System.Random            (newStdGen)
import           Yesod.Auth               (Auth, AuthPlugin (AuthPlugin),
                                           AuthRoute, Creds (Creds),
                                           Route (PluginR), YesodAuth,
                                           authHttpManager, setCredsRedirect)
import qualified Yesod.Auth.Message       as Msg
import           Yesod.Core               (HandlerSite, MonadHandler,
                                           getRouteToParent, getUrlRender,
                                           getYesod, invalidArgs, lift,
                                           lookupGetParam,
                                           lookupSession, notFound, redirect,
                                           setSession, whamlet, (.:),
                                           TypedContent, HandlerT, liftIO)

pid :: Text
pid = "googleemail2"

forwardUrl :: AuthRoute
forwardUrl = PluginR pid ["forward"]

csrfKey :: Text
csrfKey = "_GOOGLE_CSRF_TOKEN"

getCsrfToken :: MonadHandler m => m (Maybe Text)
getCsrfToken = lookupSession csrfKey

getCreateCsrfToken :: MonadHandler m => m Text
getCreateCsrfToken = do
    mtoken <- getCsrfToken
    case mtoken of
        Just token -> return token
        Nothing -> do
            stdgen <- liftIO newStdGen
            let token = T.pack $ fst $ randomString 10 stdgen
            setSession csrfKey token
            return token

authGoogleEmail :: YesodAuth m
                => Text -- ^ client ID
                -> Text -- ^ client secret
                -> AuthPlugin m
authGoogleEmail clientID clientSecret =
    AuthPlugin pid dispatch login
  where
    complete = PluginR pid ["complete"]

    getDest :: MonadHandler m
            => (Route Auth -> Route (HandlerSite m))
            -> m Text
    getDest tm = do
        csrf <- getCreateCsrfToken
        render <- getUrlRender
        let qs = map (second Just)
                [ ("scope", "email")
                , ("state", csrf)
                , ("redirect_uri", render $ tm complete)
                , ("response_type", "code")
                , ("client_id", clientID)
                , ("access_type", "offline")
                ]
        return $ decodeUtf8
               $ toByteString
               $ fromByteString "https://accounts.google.com/o/oauth2/auth"
                    `mappend` renderQueryText True qs

    login tm = do
        [whamlet|<a href=@{tm forwardUrl}>_{Msg.LoginGoogle}|]

    dispatch :: YesodAuth site
             => Text
             -> [Text]
             -> HandlerT Auth (HandlerT site IO) TypedContent
    dispatch "GET" ["forward"] = do
        tm <- getRouteToParent
        lift (getDest tm) >>= redirect

    dispatch "GET" ["complete"] = do
        mstate <- lookupGetParam "state"
        case mstate of
            Nothing -> invalidArgs ["CSRF state from Google is missing"]
            Just state -> do
                mtoken <- getCsrfToken
                unless (Just state == mtoken) $ invalidArgs ["Invalid CSRF token from Google"]
        mcode <- lookupGetParam "code"
        code <-
            case mcode of
                Nothing -> invalidArgs ["Missing code paramter"]
                Just c -> return c

        render <- getUrlRender

        req' <- liftIO $ parseUrl "https://accounts.google.com/o/oauth2/token" -- FIXME don't hardcode, use: https://accounts.google.com/.well-known/openid-configuration
        let req =
                urlEncodedBody
                    [ ("code", encodeUtf8 code)
                    , ("client_id", encodeUtf8 clientID)
                    , ("client_secret", encodeUtf8 clientSecret)
                    , ("redirect_uri", encodeUtf8 $ render complete)
                    , ("grant_type", "authorization_code")
                    ]
                    req'
                        { requestHeaders = []
                        }
        manager <- liftM authHttpManager $ lift getYesod
        res <- http req manager
        value <- responseBody res $$+- sinkParser json'
        Tokens accessToken tokenType <-
            case parseEither parseJSON value of
                Left e -> error e
                Right t -> return t

        unless (tokenType == "Bearer") $ error $ "Unknown token type: " ++ show tokenType

        req2' <- liftIO $ parseUrl "https://www.googleapis.com/plus/v1/people/me"
        let req2 = req2'
                { requestHeaders =
                    [ ("Authorization", encodeUtf8 $ "Bearer " `mappend` accessToken)
                    ]
                }
        res2 <- http req2 manager
        value2 <- responseBody res2 $$+- sinkParser json'
        Person emails <-
            case parseEither parseJSON value2 of
                Left e -> error e
                Right x -> return x
        email <-
            case map emailValue $ filter (\e -> emailType e == "account") emails of
                [e] -> return e
                [] -> error "No account email"
                x -> error $ "Too many account emails: " ++ show x
        lift $ setCredsRedirect $ Creds pid email $ allPersonInfo value2

    dispatch _ _ = notFound

data Tokens = Tokens Text Text
instance FromJSON Tokens where
    parseJSON = withObject "Tokens" $ \o -> Tokens
        <$> o .: "access_token"
        <*> o .: "token_type"

data Person = Person [Email]
instance FromJSON Person where
    parseJSON = withObject "Person" $ \o -> Person
        <$> o .: "emails"

data Email = Email
    { emailValue :: Text
    , emailType  :: Text
    }
    deriving Show
instance FromJSON Email where
    parseJSON = withObject "Email" $ \o -> Email
        <$> o .: "value"
        <*> o .: "type"

allPersonInfo :: A.Value -> [(Text, Text)]
allPersonInfo (A.Object o) = map enc $ M.toList o
    where enc (key, A.String s) = (key, s)
          enc (key, v) = (key, TL.toStrict $ TL.toLazyText $ A.encodeToTextBuilder v)
allPersonInfo _ = []
