\begin{code}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Hack.Handler.SimpleServer

data HelloWorld = HelloWorld
instance Yesod HelloWorld where
    handlers = [$resources|
/:
    Get: helloWorld
|]

helloWorld :: Handler HelloWorld TemplateFile
helloWorld = return $ TemplateFile "examples/template.html" $ cs
                [ ("title", "Hello world!")
                , ("content", "Hey look!! I'm <auto escaped>!")
                ]

main :: IO ()
main = putStrLn "Running..." >> run 3000 (toHackApp HelloWorld)
\end{code}
