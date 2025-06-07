{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import Data.Time
import Conduit
import Data.Monoid ((<>))
import Control.Concurrent.STM.Lifted
import Data.Text (Text)
import qualified Data.Map as M
import UnliftIO.Exception (try, SomeException)

data App = App (TVar (M.Map Text (TChan Text, Int)))

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

cleanupChannel :: (Eq a1, Num a1) => Maybe (a2, a1) -> Maybe (a2, a1)
cleanupChannel Nothing = Nothing
cleanupChannel (Just (writeChan, 1)) = Nothing
cleanupChannel (Just c) = Just c

userJoinedChannel :: Num b => Maybe (a, b) -> Maybe (a, b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeChan, numUsers)) = Just (writeChan, numUsers + 1)

chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name <> ". Please enter your channel ID"
    channelId <- receiveData
    sendTextData $ name <> " just joined " <> channelId

    App channelMapTVar <- getYesod

    channelMap <- readTVarIO channelMapTVar

    let maybeChan = M.lookup channelId channelMap

    writeChan <- atomically $ case maybeChan of
                                Nothing -> do
                                    chan <- newBroadcastTChan
                                    writeTVar channelMapTVar $ M.insert channelId (chan, 1) channelMap
                                    return chan
                                Just (writeChan, _) -> do
                                    writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
                                    return writeChan

    readChan <- atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan
    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan writeChan $ name <> ": " <> msg))

    atomically $ case e of
        Left _ -> do
            -- clean up your resources when user disconnects here
            let newChannelMap = M.alter cleanupChannel channelId channelMap
            writeTVar channelMapTVar newChannelMap
            writeTChan writeChan $ name <> " has left the chat"
        Right () -> return ()

getHomeR :: Handler Html
getHomeR = do
    webSockets chatApp
    defaultLayout $ do
        [whamlet|
            <div #output>
            <form #form>
                <input #input autofocus>
        |]
        toWidget [lucius|
            \#output {
                width: 600px;
                height: 400px;
                border: 1px solid black;
                margin-bottom: 1em;
                p {
                    margin: 0 0 0.5em 0;
                    padding: 0 0 0.5em 0;
                    border-bottom: 1px dashed #99aa99;
                }
            }
            \#input {
                width: 600px;
                display: block;
            }
        |]
        toWidget [julius|
            var url = document.URL,
                output = document.getElementById("output"),
                form = document.getElementById("form"),
                input = document.getElementById("input"),
                conn;

            url = url.replace("http:", "ws:").replace("https:", "wss:");
            conn = new WebSocket(url);

            conn.onmessage = function(e) {
                var p = document.createElement("p");
                p.appendChild(document.createTextNode(e.data));
                output.appendChild(p);
            };
/* *******************************************************************************************************
The following code demonstrates one way to prevent timeouts. The "if" test is added to prevent chat participants from getting the ping message “dcba” every twenty seconds. It also prevents participants from receiving any message ending with “dcba” sent by any chat participant. “ e.data.split("").reverse().join("").substring(0,4)” changes, for example, “user:abc123dcba” to “abcd321cba:resu” and grabs the first four characters; i.e., “abcd”. Messages are broadcast only if the last four characters are not “dcba”. Note that the variable "t" controls the length of the timeout period. t = 3 allows one minute of inactivity. t = 30 allows ten minutes, and t = 180 allows an hour. The value inserted below is 360 (2 hours).
*/
            conn.onmessage = function(e) {
                var p = document.createElement("p");
                p.appendChild(document.createTextNode(e.data));
                if (e.data.split("").reverse().join("").substring(0,4) != "abcd") {
                    output.appendChild(p);
                }
            };
            var t = 360
            setInterval (function () {
                t = t - 1;
                if (t > 0)
                {
                  conn.send("dcba");
                }
            }, 20000);
/* ****************************************************************************************************** */
            form.addEventListener("submit", function(e){
                conn.send(input.value);
                input.value = "";
                e.preventDefault();
            });
        |]

main :: IO ()
main = do
    channelMapTVar <- newTVarIO M.empty
    warp 3000 $ App channelMapTVar
