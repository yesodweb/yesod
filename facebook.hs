{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
import Web.Authenticate.Facebook
import Data.Object
import Data.Maybe (fromMaybe)
import Network.HTTP.Enumerator

data FB = FB Facebook
fb :: FB
fb = FB $ Facebook "134280699924829" "a7685e10c8977f5435e599aaf1d232eb"
                   "http://localhost:3000/facebook/"
mkYesod "FB" [$parseRoutes|
/ RootR GET
/facebook FacebookR GET
|]

instance Yesod FB where approot _ = "http://localhost:3000"

getRootR = do
    FB f <- getYesod
    redirectString RedirectTemporary $ getForwardUrl f ["email"]
    return ()

getFacebookR = do
    FB f <- getYesod
    code <- runFormGet' $ stringInput "code"
    at <- liftIO $ getAccessToken f code
    mreq <- runFormGet' $ maybeStringInput "req"
    let req = fromMaybe "me" mreq
    so <- liftIO $ getGraphData at req
    let so' = objToHamlet so
    hamletToRepHtml [$hamlet|
%form
    %input!type=hidden!name=code!value=$string.code$
    Request: $
    %input!type=text!name=req!value=$string.req$
    \ $
    %input!type=submit
%hr
^so'^
|]

main = withHttpEnumerator $ basicHandler 3000 fb

objToHamlet :: StringObject -> Hamlet url
objToHamlet (Scalar s) = [$hamlet|$string.s$|]
objToHamlet (Sequence list) = [$hamlet|
%ul
    $forall list o
        %li ^objToHamlet.o^
|]
objToHamlet (Mapping pairs) = [$hamlet|
%dl
    $forall pairs pair
        %dt $string.fst.pair$
        %dd ^objToHamlet.snd.pair^
|]
