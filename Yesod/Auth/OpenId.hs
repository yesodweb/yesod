{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OpenId
    ( authOpenId
    , forwardUrl
    ) where

import Yesod.Auth
import qualified Web.Authenticate.OpenId as OpenId
import Control.Monad.Attempt

import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Yesod.Request
import Text.Hamlet (hamlet)
import Text.Cassius (cassius)
import Text.Blaze (toHtml)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Yesod.Auth.Message as Msg

forwardUrl :: AuthRoute
forwardUrl = PluginR "openid" ["forward"]

authOpenId :: YesodAuth m => AuthPlugin m
authOpenId =
    AuthPlugin "openid" dispatch login
  where
    complete = PluginR "openid" ["complete"]
    name = "openid_identifier"
    login tm = do
        ident <- lift newIdent
        y <- lift getYesod
        addCassius
#if GHC7
            [cassius|##{ident}
#else
            [$cassius|##{ident}
#endif
    background: #fff url(http://www.myopenid.com/static/openid-icon-small.gif) no-repeat scroll 0pt 50%;
    padding-left: 18px;
|]
        l <- lift languages
        let mr = renderMessage (getAuth 'x') y l
        addHamlet
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
<form method="get" action="@{tm forwardUrl}">
    <label for="#{ident}">OpenID: 
    <input id="#{ident}" type="text" name="#{name}" value="http://">
    <input type="submit" value="#{mr Msg.LoginOpenID}">
|]
    dispatch "GET" ["forward"] = do
        (roid, _, _) <- runFormGet $ stringInput name
        y <- getYesod
        case roid of
            FormSuccess oid -> do
                render <- getUrlRender
                toMaster <- getRouteToMaster
                let complete' = render $ toMaster complete
                res <- runAttemptT $ OpenId.getForwardUrl oid complete' Nothing []
                attempt
                  (\err -> do
                        setMessage $ toHtml $ show err
                        redirect RedirectTemporary $ toMaster LoginR
                        )
                  (redirectText RedirectTemporary)
                  res
            _ -> do
                toMaster <- getRouteToMaster
                mr <- getMessageRender
                setMessage $ mr Msg.NoOpenID
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
                setCreds True $ Creds "openid" ident []
        attempt onFailure onSuccess res
