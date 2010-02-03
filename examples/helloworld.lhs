\begin{code}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Network.Wai.Handler.SimpleServer

data HelloWorld = HelloWorld
instance Yesod HelloWorld where
    resources = [$mkResources|
/:
    Get: helloWorld
|]

helloWorld :: Handler HelloWorld ChooseRep
helloWorld = applyLayout' "Hello World" $ cs "Hello world!"

main :: IO ()
main = putStrLn "Running..." >> toWaiApp HelloWorld >>= run 3000
\end{code}
