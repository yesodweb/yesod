\begin{code}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Network.Wai.Handler.SimpleServer

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [$parseRoutes|
/ Home GET
|]

instance Yesod HelloWorld where
    approot _ = "http://localhost:3000"

getHome :: Handler HelloWorld ChooseRep
getHome = applyLayout' "Hello World" $ cs "Hello world!"

main :: IO ()
main = putStrLn "Running..." >> toWaiApp HelloWorld >>= run 3000
\end{code}
