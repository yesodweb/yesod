{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
import Yesod
import Web.Authenticate.BrowserId
import Data.Maybe (fromMaybe)
import Network.HTTP.Conduit
import Data.Text (Text)

data BID = BID
mkYesod "BID" [parseRoutes|
/ RootR GET
/complete/#Text CompleteR GET
|]

instance Yesod BID where approot = ApprootStatic "http://localhost:3000"

getRootR = defaultLayout $ do
    addScriptRemote browserIdJs
    addJulius [julius|
function bidClick() {
    navigator.id.getVerifiedEmail(function(assertion) {
        if (assertion) {
            document.location = "/complete/" + assertion;
        } else {
            alert("Invalid BrowserId login");
        }
    });
}
|]
    addHamlet [hamlet|
<p>
    <a href="javascript:bidClick();">
        <img src="https://browserid.org/i/sign_in_red.png">
|]

getCompleteR assertion = do
    memail <- withManager $ checkAssertion "localhost:3000" assertion
    defaultLayout $ addHamlet [hamlet|
<p>You tried to log in, let's see if it worked.
$maybe email <- memail
    <p>Yes it did! You are: #{email}
$nothing
    <p>Nope, sorry
|]

main = warp 3000 BID
