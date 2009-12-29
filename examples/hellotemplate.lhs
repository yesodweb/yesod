\begin{code}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Hack.Handler.SimpleServer

data HelloWorld = HelloWorld TemplateGroup
instance Yesod HelloWorld where
    handlers = [$resources|
/:
    Get: helloWorld
/groups:
    Get: helloGroup
|]

instance YesodTemplates HelloWorld where
    templates (HelloWorld g) = g

helloWorld :: Handler HelloWorld TemplateFile
helloWorld = return $ TemplateFile "examples/template.html" $ cs
                [ ("title", "Hello world!")
                , ("content", "Hey look!! I'm <auto escaped>!")
                ]

helloGroup = template "real-template" $ cs "foo"

main :: IO ()
main = do
    putStrLn "Running..."
    stg <- loadTemplates "examples"
    run 3000 (toHackApp $ HelloWorld stg)
\end{code}
