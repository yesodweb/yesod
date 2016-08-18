{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- | Use an unique id of a facebook account as an identifier via Facebook's login system.
--
-- By using this plugin, you are trusting Facebook to authenticate
-- you, and requiring users to have a Facebook account. On the plus
-- side, many users have existing Facebook accounts, the login system
-- has been long tested (as opposed to BrowserID), and it requires no
-- credential managing or setup (as opposed to Email).
--
-- In order to use this plugin:
--
-- * Create an application on the Facebook Developer platform <https://developers.facebook.com/apps>
--
-- * Create OAuth credentials. The redirect URI will be <http://yourdomain/auth/page/facebook/complete>. (If you have your authentication subsite at a different root than \/auth\/, please adjust accordingly.)
--
-- Reference for understanding the implementation: <https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow>
module Yesod.Auth.Facebook
    ( -- * Authentication handlers
      authFacebookEmail
    , authFacebookEmailSaveToken
    , forwardUrl
    -- * User authentication token
    , Token(..)
    -- * Person
    , getPerson
    , Person(..)
    , Gender(..)
    ) where

import           Yesod.Auth               (Auth, AuthPlugin (AuthPlugin),
                                           AuthRoute, Creds (Creds),
                                           Route (PluginR), YesodAuth,
                                           runHttpRequest, setCredsRedirect,
                                           logoutDest)
import qualified Yesod.Auth.Message       as Msg
import           Yesod.Core               (HandlerSite, HandlerT, MonadHandler,
                                           TypedContent, getRouteToParent,
                                           getUrlRender, invalidArgs,
                                           lift, liftIO, lookupGetParam,
                                           lookupSession, notFound, redirect,
                                           setSession, whamlet, (.:),
                                           addMessage, getYesod, authRoute,
                                           toHtml)


import           Blaze.ByteString.Builder (fromByteString, toByteString)
import           Control.Applicative      ((<$>), (<*>), empty)
import           Control.Arrow            (second)
import           Data.Monoid              ((<>))
import           Control.Monad            (unless, when)
import           Control.Monad.IO.Class   (MonadIO)
import qualified Crypto.Nonce             as Nonce
import           Data.Aeson               ((.:?))
import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode        as A
import           Data.Aeson.Parser        (json')
import           Data.Aeson.Types         (FromJSON (parseJSON), parseEither,
                                           parseMaybe, withObject, withText)
import           Data.Conduit             (($$+-), ($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import qualified Data.HashMap.Strict      as M
import           Data.Maybe               (fromMaybe, fromJust)
import           Data.Monoid              (mappend)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TL
import           Network.HTTP.Client      (Manager, parseUrl, requestHeaders,
                                           setQueryString, responseBody, 
                                           urlEncodedBody)
import           Network.HTTP.Client.Conduit (Request, bodyReaderSource)
import           Network.HTTP.Conduit (http)
import           Network.HTTP.Types       (renderQueryText)
import           System.IO.Unsafe         (unsafePerformIO)

--------------------------------------------------------------------------------
-- | An authentication token which was acquired from OAuth callback.
--   The token gets saved into the session storage only if you use
--   'authFacebookEmailSaveToken'.
--   You can acquire saved token with 'getUserAccessToken'.
--
data Token = Token { accessToken :: Text
                   , tokenType   :: Text
                   , secondsTillExpiration :: Int
                   } deriving (Show, Eq)

instance FromJSON Token where
    parseJSON = withObject "Token" $ \o -> Token
        <$> o .: "access_token"
        <*> o .: "token_type"
        <*> o .: "expires_in"


pid :: Text
pid = "facebook"

forwardUrl :: AuthRoute
forwardUrl = PluginR pid ["forward"]

csrfKey :: Text
csrfKey = "_FACEBOOK_CSRF_TOKEN"

getCsrfToken :: MonadHandler m => m (Maybe Text)
getCsrfToken = lookupSession csrfKey

accessTokenKey :: Text
accessTokenKey = "_FACEBOOK_ACCESS_TOKEN"

getCreateCsrfToken :: MonadHandler m => m Text
getCreateCsrfToken = do
    mtoken <- getCsrfToken
    case mtoken of
        Just token -> return token
        Nothing -> do
            token <- Nonce.nonce128urlT defaultNonceGen
            setSession csrfKey token
            return token

authFacebookEmail :: YesodAuth m
                => Text -- ^ client ID
                -> Text -- ^ client secret
                -> AuthPlugin m
authFacebookEmail = authPlugin False

-- | An alternative version which stores user access token in the session
--   variable. Use it if you want to request user's profile from your app.
--
authFacebookEmailSaveToken :: YesodAuth m
                         => Text -- ^ client ID
                         -> Text -- ^ client secret
                         -> AuthPlugin m
authFacebookEmailSaveToken = authPlugin True

-- Reference: https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow
--            https://developers.facebook.com/docs/facebook-login/access-tokens/
authPlugin :: YesodAuth m
           => Bool -- ^ if the token should be stored
           -> Text -- ^ client ID
           -> Text -- ^ client secret
           -> AuthPlugin m
authPlugin storeToken clientID clientSecret =
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
                [
                 ("client_id", clientID)
                , ("redirect_uri", render $ tm complete)
                , ("state", csrf)
                , ("response_type", "code")
                , ("scope", "public_profile,email")
                ]
        return $ decodeUtf8
               $ toByteString
               $ fromByteString "https://www.facebook.com/dialog/oauth"
                    `mappend` renderQueryText True qs

    login tm = do
        [whamlet|<a href=@{tm forwardUrl}>_{Msg.Facebook}|]

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
            Nothing -> invalidArgs ["CSRF state from Facebook is missing"]
            Just state -> do
                mtoken <- getCsrfToken
                unless (Just state == mtoken) $ invalidArgs ["Invalid CSRF token from Facebook"]
        mcode <- lookupGetParam "code"
        code <-
            case mcode of
                Nothing -> do
                    merr <- lookupGetParam "error"
                    case merr of
                        Nothing -> invalidArgs ["Missing code paramter"]
                        Just err -> do
                            master <- lift getYesod
                            let msg =
                                    case err of
                                        "access_denied" -> "Access denied"
                                        _ -> "Unknown error occurred: " `T.append` err
                            addMessage "error" $ toHtml msg
                            lift $ redirect $ logoutDest master
                Just c -> return c

        render <- getUrlRender

        req' <- liftIO $ parseUrl "https://graph.facebook.com/v2.3/oauth/access_token" 
        let req =
                urlEncodedBody
                    [ ("code", encodeUtf8 code)
                    , ("client_id", encodeUtf8 clientID)
                    , ("client_secret", encodeUtf8 clientSecret)
                    , ("redirect_uri", encodeUtf8 $ render complete)
                    ]
                    req'
                        { requestHeaders = []
                        }
        value <- makeHttpRequest req
        token@(Token accessToken' tokenType' expirySeconds) <-
            case parseEither parseJSON value of
                Left e -> error e
                Right t -> return t

        -- User's access token is saved for further access to API
        when storeToken $ setSession accessTokenKey accessToken'

        userIdValue <- makeHttpRequest =<< userIdRequest token
        userId <- case parseEither parseJSON userIdValue of
                    Left e -> error e
                    Right x -> return x

        personValue <- makeHttpRequest =<< personValueRequest (uid userId) token
        person <- case parseEither parseJSON personValue of
                Left e -> error e
                Right x -> return x

        lift $ setCredsRedirect $ Creds pid (personId person) $ allPersonInfo person

    dispatch _ _ = notFound

makeHttpRequest
  :: (YesodAuth site)
  => Request
  -> HandlerT Auth (HandlerT site IO) A.Value
makeHttpRequest req = lift $
    runHttpRequest req $ \res -> bodyReaderSource (responseBody res) $$ sinkParser json'

-- | Allows to fetch information about a user from Facebook's API.
--   In case of parsing error returns 'Nothing'.
--   Will throw 'HttpException' in case of network problems or error response code.
--
getPerson :: Manager -> Text -> Token -> HandlerT site IO (Maybe Person)
getPerson manager userid token = parseMaybe parseJSON <$> (do
    req <- personValueRequest userid token
    res <- http req manager
    responseBody res $$+- sinkParser json'
  )

getUserId :: Manager -> Token -> HandlerT site IO (Maybe UserId)
getUserId manager token = parseMaybe parseJSON <$> (do
    req <- userIdRequest token
    res <- http req manager
    responseBody res $$+- sinkParser json'
  )

userIdRequest :: MonadIO m => Token -> m Request
userIdRequest token = do
    req2' <- liftIO $ parseUrl "https://graph.facebook.com/v2.7/me"
    return $ setQueryString [("access_token", Just (encodeUtf8 $ accessToken token)),
                             ("fields", Just "id")] req2'

-- Reference: <https://developers.facebook.com/docs/graph-api/reference/user>
personValueRequest :: MonadIO m => Text -> Token -> m Request
personValueRequest userid token = do
    req2' <- liftIO $ parseUrl ("https://graph.facebook.com/v2.7/" <> (T.unpack userid))
    return $ setQueryString [("access_token", Just (encodeUtf8 $ accessToken token)),
                             ("fields", Just "id,email,name,gender")] req2'


data UserId = UserId {
      uid :: Text
    } deriving (Show, Eq)

instance FromJSON UserId where
    parseJSON (A.Object v) = UserId <$>
                             v .: "id"
    parseJSON _ = empty

data Person = Person {
      personId :: Text,
      name :: Text,
      email :: Maybe Text,
      gender :: Gender
}

allPersonInfo :: Person -> [(Text, Text)]
allPersonInfo person = if (email person == Nothing)
                       then info
                       else ("email", fromJust $ email person):info
    where
      info = [("name", name person),("gender", T.pack $ show $ gender person)]

data Gender = Male | Female | OtherGender deriving (Show, Eq)

instance FromJSON Gender where
    parseJSON = withText "Gender" $ \t -> return $ case t of
                                                "male"   -> Male
                                                "female" -> Female
                                                _        -> OtherGender

instance FromJSON Person where
    parseJSON (A.Object v) = Person <$>
                             v .: "id" <*>
                             v .: "name" <*>
                             v .: "email" <*>
                             v .: "gender"
    parseJSON _ = empty


defaultNonceGen :: Nonce.Generator
defaultNonceGen = unsafePerformIO (Nonce.new)
{-# NOINLINE defaultNonceGen #-}
