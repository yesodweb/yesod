{-# LANGUAGE QuasiQuotes, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Yesod.Static
import Yesod.Dispatch
import Yesod.Core
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static

staticFiles "."

data Sample = Sample
getStatic _ = Static $ defaultFileServerSettings { ssFolder = fileSystemLookup $ toFilePath "tests" }
mkYesod "Sample" [parseRoutes|
/ RootR GET
/static StaticR Static getStatic
|]
instance Yesod Sample where approot _ = ""

getRootR = do
    redirectText RedirectPermanent "static"
    return ()

main = toWaiApp Sample >>= run 3000
