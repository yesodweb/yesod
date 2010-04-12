\begin{code}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Network.Wai.Handler.SimpleServer

data HelloWorld = HelloWorld TemplateGroup

mkYesod "HelloWorld" [$parseRoutes|
/       Home  GET
/groups Group GET
|]

instance Yesod HelloWorld where
    approot _ = "http://localhost:3000"
instance YesodTemplate HelloWorld where
    getTemplateGroup (HelloWorld tg) = tg
    defaultTemplateAttribs _ _ = return
        . setHtmlAttrib "default" "<DEFAULT>"

getHome :: Handler HelloWorld RepHtml
getHome = templateHtml "template" $ return
  . setHtmlAttrib "title" "Hello world!"
  . setHtmlAttrib "content" "Hey look!! I'm <auto escaped>!"

getGroup :: YesodTemplate y => Handler y RepHtmlJson
getGroup = templateHtmlJson "real-template" (cs "bar") $ \ho ->
    return . setHtmlAttrib "foo" ho

main :: IO ()
main = do
    putStrLn "Running..."
    loadTemplateGroup "examples" >>= toWaiApp . HelloWorld >>= run 3000
\end{code}
