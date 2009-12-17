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

helloWorld :: Handler HelloWorld HtmlObject
helloWorld = return $ cs "Hello world!"

main :: IO ()
main = putStrLn "Running..." >> run 3000 (toHackApp HelloWorld)
\end{code}
