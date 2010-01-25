{-# LANGUAGE QuasiQuotes #-}
import Yesod
import Hack.Handler.SimpleServer

data I18N = I18N

instance Yesod I18N where
    handlers = [$resources|
/:
    Get: homepage
/set/$lang:
    Get: setLang
|]

homepage = do
    ls <- languages
    let hello = chooseHello ls
    return [(TypePlain, cs hello :: Content)]

chooseHello [] = "Hello"
chooseHello ("he":_) = "שלום"
chooseHello ("es":_) = "Hola"
chooseHello (_:rest) = chooseHello rest

setLang lang = do
    addCookie 1 langKey lang
    redirect RedirectTemporary "/"
    return ()

main = putStrLn "Running..." >> toHackApp I18N >>= run 3000
