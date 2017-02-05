{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
module YesodCoreTest.StubSslOnly
    ( App ( App )
    , Widget
    , resourcesApp
    ) where

import Yesod.Core
import qualified Web.ClientSession                  as CS

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
    yesodMiddleware = defaultYesodMiddleware . (sslOnlyMiddleware 120)
    makeSessionBackend _ = sslOnlySessions $
        fmap Just $ defaultClientSessionBackend 120 CS.defaultKeyFile

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            Welcome to my test application.
    |]
