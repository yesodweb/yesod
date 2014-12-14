{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
module YesodCoreTest.StubUnsecured ( App ( App ) ) where

import Yesod.Core

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            Welcome to my test application.
    |]
