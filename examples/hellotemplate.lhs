\begin{code}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Hack.Handler.SimpleServer

data HelloWorld = HelloWorld
instance Yesod HelloWorld where
    handlers = [$resources|
/:
    Get: helloWorld
/groups:
    Get: helloGroup
|]
    templateDir _ = "examples"

helloWorld :: Handler HelloWorld TemplateFile
helloWorld = return $ TemplateFile "examples/template.html" $ cs
                [ ("title", "Hello world!")
                , ("content", "Hey look!! I'm <auto escaped>!")
                ]

helloGroup :: Handler y Template
helloGroup = template "real-template" "foo" (cs "bar") $ return []

main :: IO ()
main = do
    putStrLn "Running..."
    run 3000 $ toHackApp HelloWorld
\end{code}
