{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod.Core
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Control.Concurrent.Lifted (threadDelay)
import qualified Data.Text as T
import Control.Monad (forM_)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

getHomeR :: Handler TypedContent
getHomeR = do
    value <- lookupGetParam "x"
    case value of
        Just "file" -> respondSource typePlain $ do
            sendChunkText "Going to read a file\n\n"
            CB.sourceFile "streaming.hs" $= awaitForever sendChunkBS
            sendChunkText "Finished reading the file\n"
        Just "fibs" -> respondSource typePlain $ do
            forM_ fibs $ \fib -> do
                $logError $ "Got fib: " <> T.pack (show fib)
                sendChunkText $ "Next fib is: " <> T.pack (show fib) <> "\n"
                yield Flush
                sendFlush
                threadDelay 1000000
        _ -> fmap toTypedContent $ defaultLayout $ do
            setTitle "Streaming"
            [whamlet|
                <p>Notice how in the code above we perform selection before starting the stream.
                <p>Anyway, choose one of the options below.
                <ul>
                    <li>
                        <a href=?x=file>Read a file
                    <li>
                        <a href=?x=fibs>See the fibs
            |]

main = warp 3000 App
