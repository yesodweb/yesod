{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
import Yesod.Auth
import Yesod.Form
import Yesod.Auth.Kerberos

data Kerberos = Kerberos

mkYesod "Kerberos" [parseRoutes|
/ RootR GET
/after AfterLoginR GET
/auth AuthR Auth getAuth
|]

getRootR :: Handler ()
getRootR = redirect RedirectTemporary $ AuthR LoginR

getAfterLoginR :: Handler RepHtml
getAfterLoginR = defaultLayout $ return ()

instance Yesod Kerberos where
    approot _ = "http://localhost:3000"

instance YesodAuth Kerberos where
    type AuthId Kerberos = String
    loginDest _ = AfterLoginR
    logoutDest _ = AuthR LoginR
    getAuthId _ = do
        liftIO $ putStrLn "getAuthId"
        return $ Just "foo"
    authPlugins = [authKerberos]

instance RenderMessage Kerberos FormMessage where
    renderMessage _ _ = defaultFormMessage

main :: IO ()
main = warpDebug 3000 Kerberos
