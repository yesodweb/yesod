{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import           AppCache
import           Routes
import           Yesod.Core

instance Yesod App

mkYesodDispatch "App" resourcesApp

getHomeR :: Handler String
getHomeR = return "Hello"

getSomethingR :: Handler String
getSomethingR = return "Hello"

getAppCacheR :: Handler AppCache
getAppCacheR = $(appCache resourcesApp)

main :: IO ()
main = warp 3000 App
