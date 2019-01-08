{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Time
import Data.Conduit
import qualified Data.Conduit.List

data App = App

instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

timeSource :: MonadIO m => ConduitT () TL.Text m ()
timeSource = forever $ do
    now <- liftIO getCurrentTime
    yield $ TL.pack $ show now
    liftIO $ threadDelay 5000000

getHomeR :: Handler Html
getHomeR = do
    webSockets $ race_
        (runConduit (sourceWS .| Data.Conduit.List.map TL.toUpper .| sinkWSText))
        (runConduit (timeSource .| sinkWSText))
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
                conn.onclose = function () {
                    document.write("<p>Connection Closed</p>");
                };
            |]

main :: IO ()
main = warp 3000 App
