{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
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
-- @since 1.3.1
module Yesod.Auth.GoogleEmail2
    ( -- * Authentication handlers
      authGoogleEmail
    , authGoogleEmailSaveToken
    , forwardUrl
    -- * User authentication token
    , Token(..)
    , getUserAccessToken
    -- * Person
    , getPerson
    , Person(..)
    , Name(..)
    , Gender(..)
    , PersonImage(..)
    , resizePersonImage
    , RelationshipStatus(..)
    , PersonURI(..)
    , PersonURIType(..)
    , Organization(..)
    , OrganizationType(..)
    , Place(..)
    , Email(..)
    , EmailType(..)
    -- * Other functions
    , pid
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
                                           addMessage, getYesod,
                                           toHtml)


import           Blaze.ByteString.Builder (fromByteString, toByteString)
import           Control.Applicative      ((<$>), (<*>))
import           Control.Arrow            (second)
import           Control.Monad            (unless, when)
import           Control.Monad.IO.Class   (MonadIO)
import qualified Crypto.Nonce             as Nonce
import           Data.Aeson               ((.:?))
import qualified Data.Aeson               as A
#if MIN_VERSION_aeson(1,0,0)
import qualified Data.Aeson.Text          as A
#else
import qualified Data.Aeson.Encode        as A
#endif
import           Data.Aeson.Parser        (json')
import           Data.Aeson.Types         (FromJSON (parseJSON), parseEither,
                                           parseMaybe, withObject, withText)
import           Data.Conduit             (($$+-), ($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import qualified Data.HashMap.Strict      as M
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              (mappend)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TL
import           Network.HTTP.Client      (Manager, requestHeaders,
                                           responseBody, urlEncodedBody)
import qualified Network.HTTP.Client      as HTTP
import           Network.HTTP.Client.Conduit (Request, bodyReaderSource)
import           Network.HTTP.Conduit (http)
import           Network.HTTP.Types       (renderQueryText)
import           System.IO.Unsafe         (unsafePerformIO)


-- | Plugin identifier. This is used to identify the plugin used for
-- authentication. The 'credsPlugin' will contain this value when this
-- plugin is used for authentication.
-- @since 1.4.17
pid :: Text
pid = "googleemail2"

forwardUrl :: AuthRoute
forwardUrl = PluginR pid ["forward"]

csrfKey :: Text
csrfKey = "_GOOGLE_CSRF_TOKEN"

getCsrfToken :: MonadHandler m => m (Maybe Text)
getCsrfToken = lookupSession csrfKey

accessTokenKey :: Text
accessTokenKey = "_GOOGLE_ACCESS_TOKEN"

-- | Get user's access token from the session. Returns Nothing if it's not found
--   (probably because the user is not logged in via 'Yesod.Auth.GoogleEmail2'
--   or you are not using 'authGoogleEmailSaveToken')
getUserAccessToken :: MonadHandler m => m (Maybe Token)
getUserAccessToken = fmap (\t -> Token t "Bearer") <$> lookupSession accessTokenKey

getCreateCsrfToken :: MonadHandler m => m Text
getCreateCsrfToken = do
    mtoken <- getCsrfToken
    case mtoken of
        Just token -> return token
        Nothing -> do
            token <- Nonce.nonce128urlT defaultNonceGen
            setSession csrfKey token
            return token

authGoogleEmail :: YesodAuth m
                => Text -- ^ client ID
                -> Text -- ^ client secret
                -> AuthPlugin m
authGoogleEmail = authPlugin False

-- | An alternative version which stores user access token in the session
--   variable. Use it if you want to request user's profile from your app.
--
-- @since 1.4.3
authGoogleEmailSaveToken :: YesodAuth m
                         => Text -- ^ client ID
                         -> Text -- ^ client secret
                         -> AuthPlugin m
authGoogleEmailSaveToken = authPlugin True

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
                [ ("scope", "email profile")
                , ("state", csrf)
                , ("redirect_uri", render $ tm complete)
                , ("response_type", "code")
                , ("client_id", clientID)
                , ("access_type", "offline")
                ]
        return $ decodeUtf8
               $ toByteString
               $ fromByteString "https://accounts.google.com/o/oauth2/auth"
                    `Data.Monoid.mappend` renderQueryText True qs

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

        req' <- liftIO $
#if MIN_VERSION_http_client(0,4,30)
            HTTP.parseUrlThrow
#else
            HTTP.parseUrl
#endif
            "https://accounts.google.com/o/oauth2/token" -- FIXME don't hardcode, use: https://accounts.google.com/.well-known/openid-configuration
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
        value <- makeHttpRequest req
        token@(Token accessToken' tokenType') <-
            case parseEither parseJSON value of
                Left e -> error e
                Right t -> return t

        unless (tokenType' == "Bearer") $ error $ "Unknown token type: " ++ show tokenType'

        -- User's access token is saved for further access to API
        when storeToken $ setSession accessTokenKey accessToken'

        personValue <- makeHttpRequest =<< personValueRequest token
        person <- case parseEither parseJSON personValue of
                Left e -> error e
                Right x -> return x

        email <-
            case map emailValue $ filter (\e -> emailType e == EmailAccount) $ personEmails person of
                [e] -> return e
                [] -> error "No account email"
                x -> error $ "Too many account emails: " ++ show x
        lift $ setCredsRedirect $ Creds pid email $ allPersonInfo personValue

    dispatch _ _ = notFound

makeHttpRequest
  :: (YesodAuth site)
  => Request
  -> HandlerT Auth (HandlerT site IO) A.Value
makeHttpRequest req = lift $
    runHttpRequest req $ \res -> bodyReaderSource (responseBody res) $$ sinkParser json'

-- | Allows to fetch information about a user from Google's API.
--   In case of parsing error returns 'Nothing'.
--   Will throw 'HttpException' in case of network problems or error response code.
--
-- @since 1.4.3
getPerson :: Manager -> Token -> HandlerT site IO (Maybe Person)
getPerson manager token = parseMaybe parseJSON <$> (do
    req <- personValueRequest token
    res <- http req manager
    responseBody res $$+- sinkParser json'
  )

personValueRequest :: MonadIO m => Token -> m Request
personValueRequest token = do
    req2' <- liftIO $
#if MIN_VERSION_http_client(0,4,30)
            HTTP.parseUrlThrow
#else
            HTTP.parseUrl
#endif
        "https://www.googleapis.com/plus/v1/people/me"
    return req2'
            { requestHeaders =
                [ ("Authorization", encodeUtf8 $ "Bearer " `mappend` accessToken token)
                ]
            }

--------------------------------------------------------------------------------
-- | An authentication token which was acquired from OAuth callback.
--   The token gets saved into the session storage only if you use
--   'authGoogleEmailSaveToken'.
--   You can acquire saved token with 'getUserAccessToken'.
--
-- @since 1.4.3
data Token = Token { accessToken :: Text
                   , tokenType   :: Text
                   } deriving (Show, Eq)

instance FromJSON Token where
    parseJSON = withObject "Tokens" $ \o -> Token
        Control.Applicative.<$> o .: "access_token"
        Control.Applicative.<*> o .: "token_type"

--------------------------------------------------------------------------------
-- | Gender of the person
--
-- @since 1.4.3
data Gender = Male | Female | OtherGender deriving (Show, Eq)

instance FromJSON Gender where
    parseJSON = withText "Gender" $ \t -> return $ case t of
                                                "male"   -> Male
                                                "female" -> Female
                                                _        -> OtherGender

--------------------------------------------------------------------------------
-- | URIs specified in the person's profile
--
-- @since 1.4.3
data PersonURI =
    PersonURI { uriLabel :: Maybe Text
              , uriValue :: Maybe Text
              , uriType  :: Maybe PersonURIType
              } deriving (Show, Eq)

instance FromJSON PersonURI where
    parseJSON = withObject "PersonURI" $ \o -> PersonURI <$> o .:? "label"
                                                         <*> o .:? "value"
                                                         <*> o .:? "type"

--------------------------------------------------------------------------------
-- | The type of URI
--
-- @since 1.4.3
data PersonURIType = OtherProfile       -- ^ URI for another profile
                   | Contributor        -- ^ URI to a site for which this person is a contributor
                   | Website            -- ^ URI for this Google+ Page's primary website
                   | OtherURI           -- ^ Other URL
                   | PersonURIType Text -- ^ Something else
                   deriving (Show, Eq)

instance FromJSON PersonURIType where
    parseJSON = withText "PersonURIType" $ \t -> return $ case t of
            "otherProfile" -> OtherProfile
            "contributor"  -> Contributor
            "website"      -> Website
            "other"        -> OtherURI
            _              -> PersonURIType t

--------------------------------------------------------------------------------
-- | Current or past organizations with which this person is associated
--
-- @since 1.4.3
data Organization =
    Organization { orgName      :: Maybe Text
                   -- ^ The person's job title or role within the organization
                 , orgTitle     :: Maybe Text
                 , orgType      :: Maybe OrganizationType
                   -- ^ The date that the person joined this organization.
                 , orgStartDate :: Maybe Text
                   -- ^ The date that the person left this organization.
                 , orgEndDate   :: Maybe Text
                   -- ^ If @True@, indicates this organization is the person's
                   -- ^ primary one, which is typically interpreted as the current one.
                 , orgPrimary   :: Maybe Bool
                 } deriving (Show, Eq)

instance FromJSON Organization where
    parseJSON = withObject "Organization" $ \o ->
        Organization <$> o .:? "name"
                     <*> o .:? "title"
                     <*> o .:? "type"
                     <*> o .:? "startDate"
                     <*> o .:? "endDate"
                     <*> o .:? "primary"

--------------------------------------------------------------------------------
-- | The type of an organization
--
-- @since 1.4.3
data OrganizationType = Work
                      | School
                      | OrganizationType Text -- ^ Something else
                      deriving (Show, Eq)
instance FromJSON OrganizationType where
    parseJSON = withText "OrganizationType" $ \t -> return $ case t of
                                                "work"   -> Work
                                                "school" -> School
                                                _        -> OrganizationType t

--------------------------------------------------------------------------------
-- | A place where the person has lived or is living at the moment.
--
-- @since 1.4.3
data Place =
    Place { -- | A place where this person has lived. For example: "Seattle, WA", "Near Toronto".
            placeValue   :: Maybe Text
            -- | If @True@, this place of residence is this person's primary residence.
          , placePrimary :: Maybe Bool
          } deriving (Show, Eq)

instance FromJSON Place where
    parseJSON = withObject "Place" $ \o -> Place <$> (o .:? "value") <*> (o .:? "primary")

--------------------------------------------------------------------------------
-- | Individual components of a name
--
-- @since 1.4.3
data Name =
    Name { -- | The full name of this person, including middle names, suffixes, etc
           nameFormatted       :: Maybe Text
           -- | The family name (last name) of this person
         , nameFamily          :: Maybe Text
           -- | The given name (first name) of this person
         , nameGiven           :: Maybe Text
           -- | The middle name of this person.
         , nameMiddle          :: Maybe Text
           -- | The honorific prefixes (such as "Dr." or "Mrs.") for this person
         , nameHonorificPrefix :: Maybe Text
           -- | The honorific suffixes (such as "Jr.") for this person
         , nameHonorificSuffix :: Maybe Text
         } deriving (Show, Eq)

instance FromJSON Name where
    parseJSON = withObject "Name" $ \o -> Name <$> o .:? "formatted"
                                               <*> o .:? "familyName"
                                               <*> o .:? "givenName"
                                               <*> o .:? "middleName"
                                               <*> o .:? "honorificPrefix"
                                               <*> o .:? "honorificSuffix"

--------------------------------------------------------------------------------
-- | The person's relationship status.
--
-- @since 1.4.3
data RelationshipStatus = Single              -- ^ Person is single
                        | InRelationship      -- ^ Person is in a relationship
                        | Engaged             -- ^ Person is engaged
                        | Married             -- ^ Person is married
                        | Complicated         -- ^ The relationship is complicated
                        | OpenRelationship    -- ^ Person is in an open relationship
                        | Widowed             -- ^ Person is widowed
                        | DomesticPartnership -- ^ Person is in a domestic partnership
                        | CivilUnion          -- ^ Person is in a civil union
                        | RelationshipStatus Text -- ^ Something else
                        deriving (Show, Eq)

instance FromJSON RelationshipStatus where
    parseJSON = withText "RelationshipStatus" $ \t -> return $ case t of
                  "single"                    -> Single
                  "in_a_relationship"         -> InRelationship
                  "engaged"                   -> Engaged
                  "married"                   -> Married
                  "its_complicated"           -> Complicated
                  "open_relationship"         -> OpenRelationship
                  "widowed"                   -> Widowed
                  "in_domestic_partnership"   -> DomesticPartnership
                  "in_civil_union"            -> CivilUnion
                  _                           -> RelationshipStatus t

--------------------------------------------------------------------------------
-- | The URI of the person's profile photo.
--
-- @since 1.4.3
newtype PersonImage = PersonImage { imageUri :: Text } deriving (Show, Eq)

instance FromJSON PersonImage where
    parseJSON = withObject "PersonImage" $ \o -> PersonImage <$> o .: "url"

-- | @resizePersonImage img 30@ would set query part to @?sz=30@ which would resize
--   the image under the URI. If for some reason you need to modify the query
--   part, you should do it after resizing.
--
-- @since 1.4.3
resizePersonImage :: PersonImage -> Int -> PersonImage
resizePersonImage (PersonImage uri) size =
    PersonImage $ uri `mappend` "?sz=" `mappend` T.pack (show size)

--------------------------------------------------------------------------------
-- | Information about the user
--   Full description of the resource https://developers.google.com/+/api/latest/people
--
-- @since 1.4.3
data Person = Person
    { personId                 :: Text
      -- | The name of this person, which is suitable for display
    , personDisplayName        :: Maybe Text
    , personName               :: Maybe Name
    , personNickname           :: Maybe Text
    , personBirthday           :: Maybe Text -- ^ Birthday formatted as YYYY-MM-DD
    , personGender             :: Maybe Gender
    , personProfileUri         :: Maybe Text -- ^ The URI of this person's profile
    , personImage              :: Maybe PersonImage
    , personAboutMe            :: Maybe Text -- ^ A short biography for this person
    , personRelationshipStatus :: Maybe RelationshipStatus
    , personUris               :: [PersonURI]
    , personOrganizations      :: [Organization]
    , personPlacesLived        :: [Place]
    -- | The brief description of this person
    , personTagline            :: Maybe Text
    -- | Whether this user has signed up for Google+
    , personIsPlusUser         :: Maybe Bool
    -- | The "bragging rights" line of this person
    , personBraggingRights     :: Maybe Text
    -- | if a Google+ page, the number of people who have +1'd this page
    , personPlusOneCount       :: Maybe Int
    -- | For followers who are visible, the number of people who have added
    --   this person or page to a circle.
    , personCircledByCount     :: Maybe Int
    -- | Whether the person or Google+ Page has been verified. This is used only
    --   for pages with a higher risk of being impersonated or similar. This
    --   flag will not be present on most profiles.
    , personVerified           :: Maybe Bool
    -- | The user's preferred language for rendering.
    , personLanguage           :: Maybe Text
    , personEmails             :: [Email]
    , personDomain             :: Maybe Text
    , personOccupation         :: Maybe Text -- ^ The occupation of this person
    , personSkills             :: Maybe Text -- ^ The person's skills
    } deriving (Show, Eq)


instance FromJSON Person where
    parseJSON = withObject "Person" $ \o ->
        Person <$> o .:  "id"
               <*> o .:  "displayName"
               <*> o .:? "name"
               <*> o .:? "nickname"
               <*> o .:? "birthday"
               <*> o .:? "gender"
               <*> (o .:? "url")
               <*> o .:? "image"
               <*> o .:? "aboutMe"
               <*> o .:? "relationshipStatus"
               <*> ((fromMaybe []) <$> (o .:? "urls"))
               <*> ((fromMaybe []) <$> (o .:? "organizations"))
               <*> ((fromMaybe []) <$> (o .:? "placesLived"))
               <*> o .:? "tagline"
               <*> o .:? "isPlusUser"
               <*> o .:? "braggingRights"
               <*> o .:? "plusOneCount"
               <*> o .:? "circledByCount"
               <*> o .:? "verified"
               <*> o .:? "language"
               <*> ((fromMaybe []) <$> (o .:? "emails"))
               <*> o .:? "domain"
               <*> o .:? "occupation"
               <*> o .:? "skills"

--------------------------------------------------------------------------------
-- | Person's email
--
-- @since 1.4.3
data Email = Email
    { emailValue :: Text
    , emailType  :: EmailType
    }
    deriving (Show, Eq)

instance FromJSON Email where
    parseJSON = withObject "Email" $ \o -> Email
        <$> o .: "value"
        <*> o .: "type"

--------------------------------------------------------------------------------
-- | Type of email
--
-- @since 1.4.3
data EmailType = EmailAccount   -- ^ Google account email address
               | EmailHome      -- ^ Home email address
               | EmailWork      -- ^ Work email adress
               | EmailOther     -- ^ Other email address
               | EmailType Text -- ^ Something else
               deriving (Show, Eq)

instance FromJSON EmailType where
    parseJSON = withText "EmailType" $ \t -> return $ case t of
        "account" -> EmailAccount
        "home"    -> EmailHome
        "work"    -> EmailWork
        "other"   -> EmailOther
        _         -> EmailType t

allPersonInfo :: A.Value -> [(Text, Text)]
allPersonInfo (A.Object o) = map enc $ M.toList o
    where enc (key, A.String s) = (key, s)
          enc (key, v) = (key, TL.toStrict $ TL.toLazyText $ A.encodeToTextBuilder v)
allPersonInfo _ = []


-- See https://github.com/yesodweb/yesod/issues/1245 for discussion on this
-- use of unsafePerformIO.
defaultNonceGen :: Nonce.Generator
defaultNonceGen = unsafePerformIO (Nonce.new)
{-# NOINLINE defaultNonceGen #-}
