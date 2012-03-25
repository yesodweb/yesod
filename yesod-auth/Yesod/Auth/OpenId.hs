{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OpenId
    ( authOpenId
    , authOpenIdExtended
    , forwardUrl
    ) where

import Yesod.Auth
import qualified Web.Authenticate.OpenId as OpenId

import Yesod.Form
import Yesod.Handler
import Yesod.Widget (toWidget, whamlet)
import Yesod.Request
import Text.Cassius (cassius)
import Text.Blaze (toHtml)
import Data.Text (Text)
import qualified Yesod.Auth.Message as Msg
import Control.Exception.Lifted (SomeException, try)

forwardUrl :: AuthRoute
forwardUrl = PluginR "openid" ["forward"]

authOpenId :: YesodAuth m => AuthPlugin m
authOpenId = authOpenIdExtended []

authOpenIdExtended :: YesodAuth m => [(Text, Text)] -> AuthPlugin m
authOpenIdExtended extensionFields =
    AuthPlugin "openid" dispatch login
  where
    complete = PluginR "openid" ["complete"]
    name = "openid_identifier"
    login tm = do
        ident <- lift newIdent
        toWidget [cassius|##{ident}
    background: #fff url(http://www.myopenid.com/static/openid-icon-small.gif) no-repeat scroll 0pt 50%;
    padding-left: 18px;
|]
        [whamlet|
<form method="get" action="@{tm forwardUrl}">
    <input type="hidden" name="openid_identifier" value="https://www.google.com/accounts/o8/id">
    <button .openid-google>_{Msg.LoginGoogle}
<form method="get" action="@{tm forwardUrl}">
    <input type="hidden" name="openid_identifier" value="http://me.yahoo.com">
    <button .openid-yahoo>_{Msg.LoginYahoo}
<form method="get" action="@{tm forwardUrl}">
    <label for="#{ident}">OpenID: #
    <input id="#{ident}" type="text" name="#{name}" value="http://">
    <input type="submit" value="_{Msg.LoginOpenID}">
|]
    dispatch "GET" ["forward"] = do
        roid <- runInputGet $ iopt textField name
        case roid of
            Just oid -> do
                render <- getUrlRender
                toMaster <- getRouteToMaster
                let complete' = render $ toMaster complete
                master <- getYesod
                eres <- lift $ try $ OpenId.getForwardUrl oid complete' Nothing extensionFields (authHttpManager master)
                case eres of
                    Left err -> do
                        setMessage $ toHtml $ show (err :: SomeException)
                        redirect $ toMaster LoginR
                    Right x -> redirect x
            Nothing -> do
                toMaster <- getRouteToMaster
                setMessageI Msg.NoOpenID
                redirect $ toMaster LoginR
    dispatch "GET" ["complete", ""] = dispatch "GET" ["complete"] -- compatibility issues
    dispatch "GET" ["complete"] = do
        rr <- getRequest
        completeHelper $ reqGetParams rr
    dispatch "POST" ["complete", ""] = dispatch "POST" ["complete"] -- compatibility issues
    dispatch "POST" ["complete"] = do
        (posts, _) <- runRequestBody
        completeHelper posts
    dispatch _ _ = notFound

completeHelper :: YesodAuth m => [(Text, Text)] -> GHandler Auth m ()
completeHelper gets' = do
        master <- getYesod
        eres <- lift $ try $ OpenId.authenticate gets' (authHttpManager master)
        toMaster <- getRouteToMaster
        let onFailure err = do
            setMessage $ toHtml $ show (err :: SomeException)
            redirect $ toMaster LoginR
        let onSuccess (OpenId.Identifier ident, _) =
                setCreds True $ Creds "openid" ident gets'
        either onFailure onSuccess eres
