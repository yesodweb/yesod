\begin{code}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Network.Wai.Handler.SimpleServer

data HelloWorld = HelloWorld TemplateGroup
instance YesodTemplate HelloWorld where
    getTemplateGroup (HelloWorld tg) = tg
    defaultTemplateAttribs _ = return . setHtmlAttrib "default" "<DEFAULT>"
instance Yesod HelloWorld where
    resources = [$mkResources|
/:
    Get: helloWorld
/groups:
    Get: helloGroup
|]

helloWorld :: Handler HelloWorld RepHtml
helloWorld = templateHtml "template" $ return
  . setHtmlAttrib "title" "Hello world!"
  . setHtmlAttrib "content" "Hey look!! I'm <auto escaped>!"

helloGroup :: YesodTemplate y => Handler y RepHtmlJson
helloGroup = templateHtmlJson "real-template" (cs "bar") $ \ho ->
    return . setHtmlAttrib "foo" ho

main :: IO ()
main = do
    putStrLn "Running..."
    loadTemplateGroup "examples" >>= toWaiApp . HelloWorld >>= run 3000
\end{code}
