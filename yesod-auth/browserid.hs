{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod.Core
import Yesod.Auth
import Yesod.Auth.BrowserId
import Data.Text (Text)
import Text.Hamlet (hamlet)
import Control.Monad.IO.Class (liftIO)
import Yesod.Form
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Conduit
import Network.TLS

data BID = BID { httpManager :: Manager }

mkYesod "BID" [parseRoutes|
/ RootR GET
/after AfterLoginR GET
/auth AuthR Auth getAuth
|]

getRootR :: Handler ()
getRootR = redirect $ AuthR LoginR

getAfterLoginR :: Handler RepHtml
getAfterLoginR = do
    mauth <- maybeAuthId
    defaultLayout $ toWidget [hamlet|
<p>Auth: #{show mauth}
|]

instance Yesod BID where
    approot = ApprootStatic "http://localhost:3000"

instance YesodAuth BID where
    type AuthId BID = Text
    loginDest _ = AfterLoginR
    logoutDest _ = AuthR LoginR
    getAuthId = return . Just . credsIdent
    authPlugins _ = [authBrowserId]
    authHttpManager = httpManager

instance RenderMessage BID FormMessage where
    renderMessage _ _ = defaultFormMessage

main :: IO ()
main = do
    m <- newManager def
        { managerCheckCerts = \_ _ -> return CertificateUsageAccept
        }
    toWaiApp (BID m) >>= run 3000
