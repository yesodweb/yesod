{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Wiki
import           Yesod

-- A very simple App, doesn't do anything except provide the Wiki.
data App = App
    { appWiki :: Wiki
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/wiki WikiR Wiki appWiki
|]

instance Yesod App
instance YesodWiki App -- Just use the defaults
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            Welcome to my test application.
            The application is pretty boring.
            You probably want to go to
            <a href=@{WikiR WikiHomeR}>the wiki#
            .
    |]

main :: IO ()
main = do
    app <- App <$> newWiki
    warp 3000 app
