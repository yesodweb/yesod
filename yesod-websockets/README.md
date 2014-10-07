
##README##

**REQUIRED PACKAGES:**
chat.hs requires two packages which are not part of yesod-websockets: stm-lifted and conduit-combinators. yesod-websockets can be installed with the command "cabal install". The additional packages can be installed with the command, "cabal install stm-lifted conduit-combinators".

**TIMEOUTS:** chat.hs sessions timeout after about 30 seconds of inactivity. One way to control timeouts is to add a few lines of code to chat.hs as follows:
```haskell
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
            };
/* ******************************************************************************************************* 
The following code demonstrates one way to prevent timeouts. The "if" test is added to prevent chat participants from getting the ping message “dcba” every twenty seconds. It also prevents participants from receiving any message ending with “dcba” sent by any chat participant. “ e.data.split("").reverse().join("").substring(0,4)” changes, for example, “user:abc123dcba” to “abcd321cba:resu” and grabs the first four characters; i.e., “abcd”. Messages are broadcast only if the last four characters are not “dcba”. Note that the variable "t" controls the length of the timeout period. t = 3 allows one minute of inactivity. t = 30 allows ten minutes, and t = 180 allows an hour. The value inserted below is 360 (2 hours).
START NEW CODE *********************************************************************************   */

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

/* END ******************************************************************************************** */
            form.addEventListener("submit", function(e){
                conn.send(input.value);
                input.value = "";
                e.preventDefault();
            });
        |]

main :: IO ()
```
**NOTE:** A file named "chat-with-timeout-control.hs" is included in this directory. It contains the above code, so unless the variable "t" is changed, it can remain idle for up to two hours before timing out.

