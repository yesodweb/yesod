{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
import Yesod.Auth
import Yesod.Auth.Facebook
import Web.Authenticate.Facebook
import Yesod.Form

data FB = FB Facebook
type Handler = GHandler FB FB

fb :: FB
fb = FB Facebook
    { facebookClientId = "154414801293567"
    , facebookClientSecret = "f901e124bee0d162c9188f92b939b370"
    , facebookRedirectUri = "http://localhost:3000/facebook"
    }

mkYesod "FB" [parseRoutes|
/ RootR GET
/after AfterLoginR GET
/auth AuthR Auth getAuth
|]

getRootR :: Handler ()
getRootR = redirect RedirectTemporary $ AuthR LoginR

getAfterLoginR :: Handler RepHtml
getAfterLoginR = defaultLayout $ return ()

instance Yesod FB where
    approot _ = "http://localhost:3000"

instance YesodAuth FB where
    type AuthId FB = String
    loginDest _ = AfterLoginR
    logoutDest _ = AuthR LoginR
    getAuthId _ = do
        liftIO $ putStrLn "getAuthId"
        return $ Just "foo"
    authPlugins = return $ authFacebook
        "154414801293567"
        "f901e124bee0d162c9188f92b939b370"
        []

instance RenderMessage FB FormMessage where
    renderMessage _ _ = defaultFormMessage

main :: IO ()
main = warpDebug 3000 fb
