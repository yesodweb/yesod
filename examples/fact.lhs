\begin{code}
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
            , ("result", show $ product [1..fromIntegral i])
            ]
factRedirect = do
    i <- getParam "num"
    redirect $ "../" ++ i ++ "/"
    return ()

main :: IO ()
main = putStrLn "Running..." >> run 3000 (toHackApp Fact)
\end{code}
