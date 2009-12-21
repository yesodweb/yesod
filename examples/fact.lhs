I in general recommend type signatures for everything. However, I wanted
to show in this example how it is possible to get away without the
signatures.

\begin{code}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Hack.Handler.SimpleServer

data Fact = Fact
instance Yesod Fact where
    handlers = [$resources|
/:
    Get: index
/#num:
    Get: fact
/fact:
    Get: factRedirect
|]

index = return $ StaticFile TypeHtml "examples/fact.html"
fact i = return $ toHtmlObject 
            [ ("input", show i)
            , ("result", show $ product [1..fromIntegral i :: Integer])
            ]
factRedirect = do
    i <- getParam "num"
    redirect $ "../" ++ i ++ "/"
\end{code}
In particular, the following line would be unnecesary if we had a type
signature here.
\begin{code}
    return ()

main :: IO ()
main = putStrLn "Running..." >> run 3000 (toHackApp Fact)
\end{code}
