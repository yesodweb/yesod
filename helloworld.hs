{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|/ Home GET|]
instance Yesod HelloWorld where approot _ = ""
getHome = return $ RepPlain $ toContent "Hello World!"
main = toWaiApp HelloWorld >>= basicHandler 3000
