{-# LANGUAGE QuasiQuotes #-}
import Yesod
import Yesod.Constants
import Hack.Handler.SimpleServer

data I18N = I18N

instance Yesod I18N where
    handlers = [$resources|
/:
    Get: homepage
/set/$lang:
    Get: setLang
|]

homepage = return Hello

setLang lang = do
    addCookie 1 langKey lang
    redirect "/"
    return ()

data Hello = Hello

instance HasReps Hello where
    reps = [(TypeHtml, const $ return $ Content $ return . cs . content)]
      where
        content [] = "Hello"
        content ("he":_) = "שלום"
        content ("es":_) = "Hola"
        content (_:rest) = content rest


main = putStrLn "Running..." >> run 3000 (toHackApp I18N)
