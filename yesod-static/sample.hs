{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod.Static
import Yesod.Core
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static

staticFiles "."

data Sample = Sample { getStatic :: Static }

--getStatic _ = Static $ defaultFileServerSettings { ssFolder = fileSystemLookup $ toFilePath "." }
mkYesod "Sample" [parseRoutes|
/ RootR GET
/static StaticR Static getStatic
|]
instance Yesod Sample where

getRootR = do
    redirect "static"
    return ()

main = do
    s <- static "."
    toWaiApp (Sample s) >>= run 3000
