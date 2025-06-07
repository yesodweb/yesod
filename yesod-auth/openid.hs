{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod.Core
import Yesod.Auth
import Yesod.Auth.OpenId
import Data.Text (Text)
import Text.Hamlet (hamlet)
import Control.Monad.IO.Class (liftIO)
import Yesod.Form
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Conduit

data BID = BID { httpManager :: Manager }

mkYesod "BID" [parseRoutes|
/ RootR GET
/after AfterLoginR GET
/auth AuthR Auth getAuth
|]

getRootR :: Handler RepHtml
getRootR = getAfterLoginR

getAfterLoginR :: Handler RepHtml
getAfterLoginR = do
    mauth <- maybeAuthId
    defaultLayout [whamlet|
<p>Auth: #{show mauth}
$maybe _ <- mauth
    <p>
        <a href=@{AuthR LogoutR}>Logout
$nothing
    <p>
        <a href=@{AuthR LoginR}>Login
|]

instance Yesod BID where
    approot = guessApproot

instance YesodAuth BID where
    type AuthId BID = Text
    loginDest _ = AfterLoginR
    logoutDest _ = AuthR LoginR
    getAuthId = return . Just . credsIdentClaimed
    authPlugins _ = [authOpenId Claimed []]
    authHttpManager = httpManager
    maybeAuthId = lookupSession credsKey

instance RenderMessage BID FormMessage where
    renderMessage _ _ = defaultFormMessage

main :: IO ()
main = do
    m <- newManager tlsManagerSettings
    toWaiApp (BID m) >>= run 3000
