\begin{code}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Hack.Handler.SimpleServer

data HelloWorld = HelloWorld TemplateGroup
instance YesodTemplate HelloWorld where
    getTemplateGroup (HelloWorld tg) = tg
instance Yesod HelloWorld where
    resources = [$mkResources|
/:
    Get: helloWorld
/groups:
    Get: helloGroup
|]

helloWorld :: Handler HelloWorld TemplateFile
helloWorld = return $ TemplateFile "examples/template.html" $ cs
                [ ("title", "Hello world!")
                , ("content", "Hey look!! I'm <auto escaped>!")
                ]

helloGroup :: YesodTemplate y => Handler y ChooseRep
helloGroup = template "real-template" (cs "bar") $ \ho ->
    return . setAttribute "foo" ho

main :: IO ()
main = do
    putStrLn "Running..."
    loadTemplateGroup "examples" >>= toHackApp . HelloWorld >>= run 3000
\end{code}
