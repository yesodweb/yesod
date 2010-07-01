{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
import Yesod.Widget
import Yesod.Helpers.Static

data HW = HW { hwStatic :: Static }
mkYesod "HW" [$parseRoutes|
/ RootR GET
/static StaticR Static hwStatic
|]
instance Yesod HW where approot _ = ""
wrapper h = [$hamlet|
#wrapper ^h^
%footer Brought to you by Yesod Widgets&trade;
|]
getRootR = applyLayoutW $ wrapWidget wrapper $ do
    i <- newIdent
    setTitle $ string "Hello Widgets"
    addStyle [$hamlet|\#$string.i${color:red}|]
    addStylesheet $ StaticR $ StaticRoute ["style.css"]
    addStylesheetRemote "http://localhost:3000/static/style2.css"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
    addScript $ StaticR $ StaticRoute ["script.js"]
    addBody [$hamlet|
%h1#$string.i$ Welcome to my first widget!!!
%p
    %a!href=@RootR@ Recursive link.
%p.noscript Your script did not load. :(
|]
    addHead [$hamlet|%meta!keywords=haskell|]
main = toWaiApp (HW $ fileLookupDir "static" typeByExt) >>= basicHandler 3000
