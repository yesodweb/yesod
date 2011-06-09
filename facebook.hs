{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
import Yesod
import Web.Authenticate.Facebook
import Data.Maybe (fromMaybe)
import Network.HTTP.Enumerator
import Data.Text (pack)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Text.Encoding (encodeUtf8)

data FB = FB Facebook
fb :: FB
fb = FB $ Facebook "134280699924829" "a7685e10c8977f5435e599aaf1d232eb"
                   "http://localhost:3000/facebook"
mkYesod "FB" [$parseRoutes|
/ RootR GET
/facebook FacebookR GET
|]

instance Yesod FB where approot _ = "http://localhost:3000"

getRootR = do
    FB f <- getYesod
    let s = getForwardUrl f ["email"]
    liftIO $ print ("Redirecting", s)
    redirectString RedirectTemporary s
    return ()

getFacebookR = do
    FB f <- getYesod
    code <- runFormGet' $ stringInput "code"
    at <- liftIO $ getAccessToken f code
    mreq <- runFormGet' $ maybeStringInput "req"
    let req = fromMaybe "me" mreq
    Right so <- liftIO $ getGraphData at req
    let so' = objToHamlet so
    hamletToRepHtml [$hamlet|\
<form>
    <input type="hidden" name="code" value="#{code}">
    \Request: 
    <input type="text" name="req" value="#{req}">
    \ 
    <input type="submit">
<hr>
\^{so'}
|]

main = warpDebug 3000 fb

objToHamlet :: A.Value -> Hamlet url
objToHamlet (A.String s) = [$hamlet|#{s}|]
objToHamlet (A.Array list) = [$hamlet|
<ul>
    $forall o <- V.toList list
        <li>^{objToHamlet o}
|]
objToHamlet (A.Object pairs) = [$hamlet|\
<dl>
    $forall pair <- M.toList pairs
        <dt>#{fst pair}
        <dd>^{objToHamlet $ snd pair}
|]
