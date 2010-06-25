{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
import Web.Authenticate.Facebook
import Data.Object
import Data.Maybe (fromMaybe)

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
    code <- runFormGet $ required $ input "code"
    at <- liftIO $ getAccessToken f code
    mreq <-runFormGet $ optional $ input "req"
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

main = toWaiApp fb >>= basicHandler 3000

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
