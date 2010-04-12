{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Network.Wai.Handler.SimpleServer

data I18N = I18N

mkYesod "I18N" [$parseRoutes|
/            Homepage GET
/set/$lang   SetLang  GET
|]

instance Yesod I18N where
    approot _ = "http://localhost:3000"

getHomepage :: Handler y [(ContentType, Content)]
getHomepage = do
    ls <- languages
    let hello = chooseHello ls
    return [(TypePlain, cs hello :: Content)]

chooseHello :: [Language] -> String
chooseHello [] = "Hello"
chooseHello ("he":_) = "שלום"
chooseHello ("es":_) = "Hola"
chooseHello (_:rest) = chooseHello rest

getSetLang :: String -> Handler y ()
getSetLang lang = do
    addCookie 1 langKey lang
    redirect RedirectTemporary "/"

main :: IO ()
main = putStrLn "Running..." >> toWaiApp I18N >>= run 3000
