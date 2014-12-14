{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
module YesodCoreTest.StubSslOnly ( App ( App ) ) where

import Yesod.Core

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
    securityPolicy _ = sslOnly 120

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            Welcome to my test application.
    |]
