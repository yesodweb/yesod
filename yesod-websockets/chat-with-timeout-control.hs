{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
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

data App = App (TChan Text)

instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    App writeChan <- getYesod
    readChan <- atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan
    race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan writeChan $ name <> ": " <> msg))

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
    chan <- atomically newBroadcastTChan
    warp 3000 $ App chan
