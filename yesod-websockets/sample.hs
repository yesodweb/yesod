{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import Control.Monad (forever)

data App = App

instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = do
    webSockets $ forever $ do
        msg <- receiveData
        sendTextData $ TL.toUpper msg
    defaultLayout $
        toWidget
            [julius|
                var conn = new WebSocket("ws://localhost:3000/");
                conn.onopen = function() {
                    document.write("<p>open!</p>");
                    document.write("<button id=button>Send another message</button>")
                    document.getElementById("button").addEventListener("click", function(){
                        var msg = prompt("Enter a message for the server");
                        conn.send(msg);
                    });
                    conn.send("hello world");
                };
                conn.onmessage = function(e) {
                    document.write("<p>" + e.data + "</p>");
                };
            |]

main :: IO ()
main = warp 3000 App
