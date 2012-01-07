{-# LANGUAGE QuasiQuotes, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Yesod.Static
import Yesod.Dispatch
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
    approot _ = ""
    cleanPath _ = Right -- FIXME make this unnecessary perhaps

getRootR = do
    redirectText RedirectPermanent "static"
    return ()

main = do
    s <- static "."
    toWaiApp (Sample s) >>= run 3000
