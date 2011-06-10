{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
import Yesod
import Web.Authenticate.Facebook
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.Map as M

data FB = FB Facebook
type Handler = GHandler FB FB

fb :: FB
fb = FB Facebook
    { facebookClientId = "154414801293567"
    , facebookClientSecret = "f901e124bee0d162c9188f92b939b370"
    , facebookRedirectUri = "http://localhost:3000/facebook"
    }

mkYesod "FB" [parseRoutes|
/ RootR GET
/facebook FacebookR GET
|]

instance Yesod FB where approot _ = "http://localhost:3000"

getRootR :: Handler ()
getRootR = do
    FB f <- getYesod
    let s = getForwardUrl f ["email"]
    liftIO $ print ("Redirecting" :: String, s)
    redirectText RedirectTemporary s

getFacebookR :: Handler RepHtml
getFacebookR = do
    FB f <- getYesod
    code <- runFormGet' $ stringInput "code"
    at <- liftIO $ getAccessToken f code
    liftIO $ print at
    mreq <- runFormGet' $ maybeStringInput "req"
    let req = fromMaybe "me" mreq
    Right so <- liftIO $ getGraphData at req
    let so' = objToHamlet so
    hamletToRepHtml [hamlet|\
<form>
    <input type="hidden" name="code" value="#{code}">
    \Request: 
    <input type="text" name="req" value="#{req}">
    \ 
    <input type="submit">
<hr>
\^{so'}
|]

main :: IO ()
main = warpDebug 3000 fb

objToHamlet :: A.Value -> Hamlet url
objToHamlet (A.String s) = [hamlet|#{s}|]
objToHamlet (A.Array list) = [hamlet|
<ul>
    $forall o <- V.toList list
        <li>^{objToHamlet o}
|]
objToHamlet (A.Object pairs) = [hamlet|\
<dl>
    $forall pair <- M.toList pairs
        <dt>#{fst pair}
        <dd>^{objToHamlet $ snd pair}
|]
objToHamlet (A.Number i) = [hamlet|<i>#{show i}|]
objToHamlet (A.Bool True) = [hamlet|<i>true|]
objToHamlet (A.Bool False) = [hamlet|<i>false|]
objToHamlet A.Null = [hamlet|<i>null|]
