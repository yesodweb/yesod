{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Yesod
import Yesod.Mail

import Yesod.Helpers.Auth2
import Yesod.Helpers.Auth2.OpenId
import Yesod.Helpers.Auth2.Rpxnow
import Yesod.Helpers.Auth2.Facebook
import Yesod.Helpers.Auth2.Email

import Control.Monad (join)
import Database.Persist.Sqlite
import Safe (readMay)

mkPersist [$persist|
Email
    email String Eq
    status Bool update
    verkey String null update
    password String null update
    UniqueEmail email
|]

data A2 = A2 { connPool :: ConnectionPool }
mkYesod "A2" [$parseRoutes|
/auth AuthR Auth getAuth
|]
instance Yesod A2 where approot _ = "http://localhost:3000"
instance YesodAuth A2 where
    type AuthId A2 = String
    loginDest _ = AuthR CheckR
    logoutDest _ = AuthR CheckR
    getAuthId = return . Just . credsIdent
    showAuthId = const id
    readAuthId = const Just
    authPlugins =
        [ authDummy
        , authOpenId
        , authRpxnow "yesod-test" "c8043882f14387d7ad8dfc99a1a8dab2e028f690"
        , authFacebook
            "d790dfc0203e31c0209ed32f90782c31"
            "a7685e10c8977f5435e599aaf1d232eb"
            []
        , authEmail
        ]

main :: IO ()
main = withConnectionPool $ \p -> do
    flip runConnectionPool p $ runMigration $ migrate (undefined :: Email)
    basicHandler 3000 $ A2 p

instance YesodAuthEmail A2 where
    type AuthEmailId A2 = EmailId
    showAuthEmailId _ = show
    readAuthEmailId _ = readMay

    addUnverified email verkey = runDB $ insert $ Email email False (Just verkey) Nothing
    sendVerifyEmail email verkey verurl = do
        render <- getUrlRenderParams
        tm <- getRouteToMaster
        let lbs = renderHamlet render [$hamlet|
%p
    %a!href=$verurl$ Verify your email address.
|]
        liftIO $ renderSendMail Mail
            { mailHeaders =
                [ ("To", email)
                , ("From", "reply@orangeroster.com")
                , ("Subject", "OrangeRoster: Verify your email address")
                ]
            , mailPlain = verurl
            , mailParts =
                [ Part
                    { partType = "text/html; charset=utf-8"
                    , partEncoding = None
                    , partDisposition = Inline
                    , partContent = lbs
                    }
                ]
            }
    getVerifyKey emailid = runDB $ do
        x <- get $ fromIntegral emailid
        return $ maybe Nothing emailVerkey x
    setVerifyKey emailid verkey = runDB $
        update (fromIntegral emailid) [EmailVerkey $ Just verkey]
    verifyAccount emailid' = runDB $ do
        let emailid = fromIntegral emailid'
        x <- get emailid
        uid <-
            case x of
                Nothing -> return Nothing
                Just email -> do
                    update emailid [EmailStatus True]
                    return $ Just $ emailEmail email
        return uid
    getPassword email = runDB $ do
        x <- getBy $ UniqueEmail email
        return $ x >>= emailPassword . snd
    setPassword email password = runDB $
        updateWhere [EmailEmailEq email] [EmailPassword $ Just password]
    getEmailCreds email = runDB $ do
        x <- getBy $ UniqueEmail email
        case x of
            Nothing -> return Nothing
            Just (eid, e) ->
                return $ Just EmailCreds
                    { emailCredsId = fromIntegral eid
                    , emailCredsAuthId = Just $ emailEmail e
                    , emailCredsStatus = emailStatus e
                    , emailCredsVerkey = emailVerkey e
                    }
    getEmail emailid = runDB $ do
        x <- get $ fromIntegral emailid
        return $ fmap emailEmail x

instance YesodPersist A2 where
    type YesodDB A2 = SqlPersist
    runDB db = fmap connPool getYesod >>= runConnectionPool db

withConnectionPool :: MonadCatchIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool "auth2.db3" 10

runConnectionPool :: MonadCatchIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool
