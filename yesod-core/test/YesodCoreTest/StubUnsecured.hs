{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
module YesodCoreTest.StubUnsecured ( App ( App ) ) where

import Yesod.Core
import Text.Hamlet (hamlet)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ toWidget
    [hamlet|
        <p>
            Welcome to my test application.
    |]
