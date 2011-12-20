{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OpenId
    ( authOpenId
    , authOpenIdExtended
    , forwardUrl
    ) where

#include "qq.h"

import Yesod.Auth
import qualified Web.Authenticate.OpenId as OpenId
import Control.Monad.Attempt

import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Yesod.Request
import Text.Cassius (cassius)
import Text.Blaze (toHtml)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Yesod.Auth.Message as Msg

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
        addCassius
            [QQ(cassius)|##{ident}
    background: #fff url(http://www.myopenid.com/static/openid-icon-small.gif) no-repeat scroll 0pt 50%;
    padding-left: 18px;
|]
        [QQ(whamlet)|
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
                res <- runAttemptT $ OpenId.getForwardUrl oid complete' Nothing extensionFields 
                attempt
                  (\err -> do
                        setMessage $ toHtml $ show err
                        redirect RedirectTemporary $ toMaster LoginR
                        )
                  (redirectText RedirectTemporary)
                  res
            Nothing -> do
                toMaster <- getRouteToMaster
                setMessageI Msg.NoOpenID
                redirect RedirectTemporary $ toMaster LoginR
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
        res <- runAttemptT $ OpenId.authenticate gets'
        toMaster <- getRouteToMaster
        let onFailure err = do
            setMessage $ toHtml $ show err
            redirect RedirectTemporary $ toMaster LoginR
        let onSuccess (OpenId.Identifier ident, _) =
                setCreds True $ Creds "openid" ident gets'
        attempt onFailure onSuccess res
