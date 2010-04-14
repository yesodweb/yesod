{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Yesod.Helpers.Static
import Network.Wai.Handler.SimpleServer

data StaticExample = StaticExample

mkYesod "StaticExample" [$parseRoutes|
/     Root    StaticRoutes siteStaticRoutes getStaticSite
|]

instance Yesod StaticExample where
    approot _ = "http://localhost:3000"

getStaticSite :: StaticExample -> Static
getStaticSite _ = fileLookupDir "dist/doc/html/yesod"

main :: IO ()
main = do
    putStrLn "Running..."
    toWaiApp StaticExample >>= run 3000
