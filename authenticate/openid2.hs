{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
import Yesod.Core
import Web.Authenticate.OpenId
import qualified Web.Authenticate.OpenId.Providers as P
import Network.HTTP.Conduit
import Yesod.Form
import Network.Wai.Handler.Warp (run)
import Text.Lucius (lucius)

data OID = OID
mkYesod "OID" [parseRoutes|
/ RootR GET
/forward ForwardR GET
/complete CompleteR GET
|]

instance Yesod OID where
    approot = ApprootStatic "http://localhost:3000"

getRootR :: Handler RepHtml
getRootR = defaultLayout [whamlet|
<form action="@{ForwardR}">
    OpenId: #
    <input type="text" name="openid_identifier" value="http://">
    <input type="submit">
<form action="@{ForwardR}">
    <input type="hidden" name="openid_identifier" value=#{P.google}>
    <input type="submit" value=Google>
|]

instance RenderMessage OID FormMessage where
    renderMessage _ _ = defaultFormMessage

getForwardR :: Handler ()
getForwardR = do
    openid <- runInputGet $ ireq textField "openid_identifier"
    render <- getUrlRender
    url <- withManager $ getForwardUrl openid (render CompleteR) Nothing []
    redirect url

getCompleteR :: Handler RepHtml
getCompleteR = do
    params <- reqGetParams `fmap` getRequest
    oir <- withManager $ authenticateClaimed params
    defaultLayout $ do
        toWidget [lucius|
table {
    border-collapse: collapse;
}
th, td {
    border: 1px solid #666;
    padding: 5px;
    vertical-align: top;
}
th {
    text-align: right;
}
|]
        [whamlet|
<p>Successfully logged in.
<table>
    <tr>
        <th>OP Local
        <td>#{identifier $ oirOpLocal oir}
    <tr>
        <th>Claimed
        <td>
            $maybe c <- oirClaimed oir
                \#{identifier c}
            $nothing
                <i>none
    <tr>
        <th>Params
        <td>
            <table>
                $forall (k, v) <- oirParams oir
                    <tr>
                        <th>#{k}
                        <td>#{v}
    <tr>
        <th>GET params
        <td>
            <table>
                $forall (k, v) <- params
                    <tr>
                        <th>#{k}
                        <td>#{v}
|]

main :: IO ()
main = toWaiApp OID >>= run 3000
