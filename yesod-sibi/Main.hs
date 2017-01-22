{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad (join)
import Control.Monad.Logger (runNoLoggingT)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy.Encoding
import Data.Typeable (Typeable)
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Text.Julius (rawJS)
import Text.Shakespeare.Text (stext)
import Yesod
import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Auth.GoogleEmail2
import           Network.HTTP.Client.Conduit (Manager, newManager)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

share
  [ mkPersist
      sqlSettings
      { mpsGeneric = False
      }
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
User
    token Text
    password Text Maybe -- Password may not be set yet
    verkey Text Maybe -- Used for resetting passwords
    verified Bool
    UniqueUserToken token
    deriving Typeable
|]

data App = App {
   db :: SqlBackend,
   httpManager :: Manager
}

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App
         -- Emails will include links, so be sure to include an approot so that
         -- the links are valid!
                                 where
  approot = ApprootStatic "http://localhost:3005"
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Set up Persistent
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App conn _ <- getYesod
    runSqlConn f conn

-- Replace with Google client ID.
clientId :: Text
clientId = "1006549472525-2rl0d428vjvdcsc9qp5ct0049t1nmq37.apps.googleusercontent.com"

-- Replace with Google secret ID.
clientSecret :: Text
clientSecret = "wk06h7lbzzU05z3i9dxKGnvH"

clientSecret2 :: Text
clientSecret2 = "HeUwOiiJnj7OpV5_V_22hGd3"

clientId2 :: Text
clientId2 = "1007157966035-hdgnh7vb5f9nihaggb0er9kgne11ln38.apps.googleusercontent.com"

instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins _ = [
     authGoogleEmail clientId clientSecret
    , authGoogleEmail clientId2 clientSecret2

    ]
  -- Need to find the UserId for the given email address.

  authenticate creds =
    runDB $
    do x <- getBy $ UniqueUserToken (credsIdent creds)
       liftIO $ print $ credsExtra creds
       liftIO $ print $ credsPlugin creds
       liftIO $ print $ credsIdent creds
       case x of
         Just (Entity uid _) -> return $ Authenticated uid
         Nothing ->
           Authenticated <$>
           insert
             User
             { userToken = credsIdent creds
             , userPassword = Nothing
             , userVerkey = Nothing
             , userVerified = False
             }

  authHttpManager = httpManager

instance YesodAuthPersist App

-- Here's all of the email-specific code
-- instance YesodAuthEmail App where
--   type AuthEmailId App = UserId
--   afterPasswordRoute _ = HomeR
--   addUnverified email verkey =
--     runDB $ insert $ User email Nothing (Just verkey) False
--   sendVerifyEmail email _ verurl
--                           -- Print out to the console the verification email, for easier
--                           -- debugging.
--    = do
--     liftIO $ putStrLn $ "Copy/ Paste this URL in your browser:" ++ unpack verurl
--     -- Send email.
--     liftIO $
--       renderSendMail
--         (emptyMail $ Address Nothing "noreply")
--         { mailTo = [Address Nothing email]
--         , mailHeaders = [("Subject", "Verify your email address")]
--         , mailParts = [[textPart, htmlPart]]
--         }
--     where
--       textPart =
--         Part
--         { partType = "text/plain; charset=utf-8"
--         , partEncoding = None
--         , partFilename = Nothing
--         , partContent =
--           Data.Text.Lazy.Encoding.encodeUtf8
--             [stext|
--                     Please confirm your email address by clicking on the link below.

--                     #{verurl}

--                     Thank you
--                 |]
--         , partHeaders = []
--         }
--       htmlPart =
--         Part
--         { partType = "text/html; charset=utf-8"
--         , partEncoding = None
--         , partFilename = Nothing
--         , partContent =
--           renderHtml
--             [shamlet|
--                     <p>Please confirm your email address by clicking on the link below.
--                     <p>
--                         <a href=#{verurl}>#{verurl}
--                     <p>Thank you
--                 |]
--         , partHeaders = []
--         }
--   getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
--   needOldPassword _ = return True
--   setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]
--   verifyAccount uid =
--     runDB $
--     do mu <- get uid
--        case mu of
--          Nothing -> return Nothing
--          Just u -> do
--            update uid [UserVerified =. True]
--            return $ Just uid
--   getPassword = runDB . fmap (join . fmap userPassword) . get
--   setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
--   getEmailCreds email =
--     runDB $
--     do mu <- getBy $ UniqueUser email
--        case mu of
--          Nothing -> return Nothing
--          Just (Entity uid u) ->
--            return $
--            Just
--              EmailCreds
--              { emailCredsId = uid
--              , emailCredsAuthId = Just uid
--              , emailCredsStatus = isJust $ userPassword u
--              , emailCredsVerkey = userVerkey u
--              , emailCredsEmail = email
--              }
--   getEmail = runDB . fmap (fmap userEmail) . get

getHomeR :: Handler Html
getHomeR = do
  maid <- maybeAuthId
  defaultLayout $
    do addScriptRemote
         "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
       addScriptRemote
         "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/2.0.3/js.cookie.min.js"
       let hname = rawJS $ TE.decodeUtf8 $ CI.foldedCase defaultCsrfHeaderName
           cname = rawJS $ TE.decodeUtf8 defaultCsrfCookieName
       toWidget
         [julius|
                     $(function() {
                     var csrfCookieName = "#{cname}";
                     var hname = "#{hname}";
                     console.log(csrfCookieName, hname);

                $("#btn").click(function(){
                            var dummyData = {
                                "email": "sibi@psibi.in"
                            };
                            $.ajax({
                               url: '/auth/page/email/register',
                               contentType: 'application/json',
                               Accept: 'application/json',
                               type: 'post',
                               headers: {
                                "#{hname}": Cookies.get(csrfCookieName)
                               },
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },
                               data: JSON.stringify(dummyData)
                             });
                });

                $("#btn2").click(function(){
                            var dummyData = {
                                "email": "sibi@psibi.in"
                            };
                            $.ajax({
                               url: '/auth/page/email/forgot-password',
                               contentType: 'application/json',
                               type: 'post',
                               headers: {
                                "#{hname}": Cookies.get(csrfCookieName)
                               },
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },
                               data: JSON.stringify(dummyData)
                             });
                });

                $("#btn3").click(function(){
                            var dummyData = {
                                "email": "sibi@psibi.in",
                                "password": "sibi"
                            };
                            $.ajax({
                               url: '/auth/page/email/login',
                               contentType: 'application/json',
                               type: 'post',
                               headers: {
                                "#{hname}": Cookies.get(csrfCookieName)
                               },
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },
                               data: JSON.stringify(dummyData)
                             });
                });

                $("#btn4").click(function(){
                            var dummyData = {
                                "new": "sibi",
                                "confirm": "sibi",
                                "current": "sibi"
                            };
                            $.ajax({
                               url: '/auth/page/email/set-password',
                               contentType: 'application/json',
                               type: 'post',
                               headers: {
                                "#{hname}": Cookies.get(csrfCookieName)
                               },
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },
                               data: JSON.stringify(dummyData)
                             });
                });

                     });
             |]
       toWidget
         [whamlet|
            <p>Your current auth ID: #{show maid}
            <button #btn>Register user
            <button #btn2>Forgot password
            <button #btn3>Login
            <button #btn4>Set password

            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

main :: IO ()
main =
  runNoLoggingT $
  withSqliteConn "email2.db3" $
  \conn ->
     liftIO $
     do runSqlConn (runMigration migrateAll) conn
        man <- newManager
        warp 3005 $ App conn man
