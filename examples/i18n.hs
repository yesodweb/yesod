{-# LANGUAGE QuasiQuotes #-}
import Yesod
import Network.Wai.Handler.SimpleServer

data I18N = I18N

instance Yesod I18N where
    resources = [$mkResources|
/:
    Get: homepage
/set/$lang:
    Get: setLang
|]

homepage :: Handler y [(ContentType, Content)]
homepage = do
    ls <- languages
    let hello = chooseHello ls
    return [(TypePlain, cs hello :: Content)]

chooseHello :: [Language] -> String
chooseHello [] = "Hello"
chooseHello ("he":_) = "שלום"
chooseHello ("es":_) = "Hola"
chooseHello (_:rest) = chooseHello rest

setLang :: String -> Handler y ()
setLang lang = do
    addCookie 1 langKey lang
    redirect RedirectTemporary "/"

main :: IO ()
main = putStrLn "Running..." >> toWaiApp I18N >>= run 3000
