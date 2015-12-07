{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
import           Control.Monad (join)
import           Control.Monad.Logger (runNoLoggingT)
import           Data.Maybe (isJust)
import           Data.Yaml
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE
import           Data.Typeable (Typeable)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Mail.Mime
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Shakespeare.Text (stext)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Email
import           Network.Mail.Mime.SES
import           Data.ByteString.Char8
import           Control.Monad (mzero)
import           Network.HTTP.Client.Conduit (Manager, newManager, HasHttpManager (getHttpManager))
import           System.Exit (exitWith, ExitCode( ExitFailure ))

share [mkPersist sqlSettings { mpsGeneric = False }, mkMigrate "migrateAll"] [persistLowerCase|
User
    email Text
    password Text Maybe -- Password may not be set yet
    verkey Text Maybe -- Used for resetting passwords
    verified Bool
    UniqueUser email
    deriving Typeable
|]

data App = App
    { sqlBackend     :: SqlBackend
    , appHttpManager :: Manager
    }

instance HasHttpManager App where
  getHttpManager = appHttpManager

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    -- Emails will include links, so be sure to include an approot so that
    -- the links are valid!
    approot = ApprootStatic "http://localhost:3000"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Set up Persistent
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        App conn _ <- getYesod
        runSqlConn f conn

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [authEmail]

    -- Need to find the UserId for the given email address.
    getAuthId creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) Nothing Nothing False
        return $ Just $
            case x of
                Left (Entity userid _) -> userid -- newly added user
                Right userid -> userid -- existing user

    authHttpManager = error "Email doesn't need an HTTP manager"

instance YesodAuthPersist App

-- Here's all of the email-specific code

data SesKeys = SesKeys { accessKey :: !Text, secretKey :: !Text }

instance FromJSON SesKeys where
  parseJSON (Object v) =
    SesKeys <$> v .: "accessKey"
            <*> v .: "secretKey"
  parseJSON _ = mzero

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = HomeR

    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False

    -- Send the verification email with your SES credentials located in config/secrets.yaml
    -- NOTE: The email address you're sending from will have to be verified on SES
    sendVerifyEmail email _ verurl = do
        h <- getYesod
        sesCreds <- liftIO $ getSESCredentials

        liftIO $ renderSendMailSES (getHttpManager h) sesCreds (emptyMail $ Address Nothing "noreply@example.com")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        getSESCredentials :: IO SES
        getSESCredentials = do
            key <- getsesAccessKey
            return SES {
                sesTo = [(TE.encodeUtf8 email)],
                sesFrom = "noreply@example.com",
                sesAccessKey =  TE.encodeUtf8 $ accessKey key,
                sesSecretKey =  TE.encodeUtf8 $ secretKey key,
                sesRegion = usWest2 }
        getsesAccessKey :: IO SesKeys
        getsesAccessKey = do
            ymlConfig <- Data.ByteString.Char8.readFile "config/secrets.yaml"

            case decode ymlConfig of
                Nothing -> do Data.ByteString.Char8.putStrLn "Error while parsing secrets.yaml"; System.Exit.exitWith (ExitFailure 1)
                Just c -> return c

        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = LTE.encodeUtf8 $
                [stext|
                    Please confirm your email address by clicking on the link below.

                    #{verurl}

                    Thank you
                |]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml
                [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you
                |]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just u -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = email
                }
    getEmail = runDB . fmap (fmap userEmail) . get

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

main :: IO ()
main = runNoLoggingT $ withSqliteConn "email.db3" $ \conn -> liftIO $ do
    runSqlConn (runMigration migrateAll) conn
    httpManager <- newManager
    warp 3000 $ App conn httpManager
