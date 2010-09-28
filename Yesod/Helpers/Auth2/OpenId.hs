{-# LANGUAGE QuasiQuotes #-}
module Yesod.Helpers.Auth2.OpenId
    ( authOpenId
    ) where

import Yesod
import Yesod.Helpers.Auth2
import qualified Web.Authenticate.OpenId as OpenId
import Control.Monad.Attempt

authOpenId :: YesodAuth m => AuthPlugin m
authOpenId =
    AuthPlugin "openid" dispatch login
  where
    forward = PluginR "openid" ["forward"]
    complete = PluginR "openid" ["complete"]
    name = "openid_identifier"
    login = do
        tm <- liftHandler getRouteToMaster
        addStyle [$cassius|
#openid
    background: #fff url(http://www.myopenid.com/static/openid-icon-small.gif) no-repeat scroll 0pt 50%;
    padding-left: 18px;
|]
        addBody [$hamlet|
%form!method=post!action=@tm.forward@
    %label!for=openid OpenID: $
    %input#openid!type=text!name=$name$
    %input!type=submit!value="Login via OpenID"
|]
    dispatch "POST" ["forward"] = do
        (roid, _, _) <- runFormPost $ stringInput name
        case roid of
            FormSuccess oid -> do
                render <- getUrlRender
                toMaster <- getRouteToMaster
                let complete' = render $ toMaster complete
                res <- runAttemptT $ OpenId.getForwardUrl oid complete'
                attempt
                  (\err -> do
                        setMessage $ string $ show err
                        redirect RedirectTemporary $ toMaster LoginR)
                  (redirectString RedirectTemporary)
                  res
            _ -> do
                toMaster <- getRouteToMaster
                setMessage $ string "No OpenID identifier found"
                redirect RedirectTemporary $ toMaster LoginR
    dispatch "GET" ["complete"] = do
        rr <- getRequest
        let gets' = reqGetParams rr
        res <- runAttemptT $ OpenId.authenticate gets'
        toMaster <- getRouteToMaster
        let onFailure err = do
            setMessage $ string $ show err
            redirect RedirectTemporary $ toMaster LoginR
        let onSuccess (OpenId.Identifier ident) =
                setCreds True $ Creds "openid" ident []
        attempt onFailure onSuccess res
    dispatch _ _ = notFound
