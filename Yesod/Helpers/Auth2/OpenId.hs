{-# LANGUAGE QuasiQuotes #-}
module Yesod.Helpers.Auth2.OpenId
    ( authOpenId
    , forwardUrl
    ) where

import Yesod
import Yesod.Helpers.Auth2
import qualified Web.Authenticate.OpenId as OpenId
import Control.Monad.Attempt
import qualified Web.Authenticate.OpenId2 as OpenId2
import Control.Exception (toException)

forwardUrl :: AuthRoute
forwardUrl = PluginR "openid" ["forward"]

authOpenId :: YesodAuth m => AuthPlugin m
authOpenId =
    AuthPlugin "openid" dispatch login
  where
    complete1 = PluginR "openid" ["complete1"]
    complete2 = PluginR "openid" ["complete2"]
    name = "openid_identifier"
    login tm = do
        ident <- newIdent
        addStyle [$cassius|
#$ident$
    background: #fff url(http://www.myopenid.com/static/openid-icon-small.gif) no-repeat scroll 0pt 50%;
    padding-left: 18px;
|]
        addBody [$hamlet|
%form!method=get!action=@tm.forwardUrl@
    %label!for=openid OpenID: $
    %input#$ident$!type=text!name=$name$
    %input!type=submit!value="Login via OpenID"
|]
    dispatch "GET" ["forward"] = do
        (roid, _, _) <- runFormGet $ stringInput name
        case roid of
            FormSuccess oid -> do
                render <- getUrlRender
                toMaster <- getRouteToMaster
                let complete2' = render $ toMaster complete2
                res2 <- runAttemptT $ OpenId2.getForwardUrl oid complete2'
                msg <-
                    case res2 of
                        Failure e -> return $ toException e
                        Success url -> redirectString RedirectTemporary url
                let complete' = render $ toMaster complete1
                res <- runAttemptT $ OpenId.getForwardUrl oid complete'
                attempt
                  (\err -> do
                        setMessage $ string $ unlines
                            [ show err
                            , show $ toException msg
                            ]
                        redirect RedirectTemporary $ toMaster LoginR
                        )
                  (redirectString RedirectTemporary)
                  res
            _ -> do
                toMaster <- getRouteToMaster
                setMessage $ string "No OpenID identifier found"
                redirect RedirectTemporary $ toMaster LoginR
    dispatch "GET" ["complete1"] = completeHelper OpenId.authenticate
    dispatch "GET" ["complete2"] =
        completeHelper (fmap OpenId.Identifier . OpenId2.authenticate)
    dispatch _ _ = notFound

completeHelper
    :: YesodAuth m
    => ([(String, String)] -> AttemptT (GHandler Auth m) OpenId.Identifier)
    -> GHandler Auth m ()
completeHelper auth = do
        rr <- getRequest
        let gets' = reqGetParams rr
        res <- runAttemptT $ auth gets'
        toMaster <- getRouteToMaster
        let onFailure err = do
            setMessage $ string $ show err
            redirect RedirectTemporary $ toMaster LoginR
        let onSuccess (OpenId.Identifier ident) =
                setCreds True $ Creds "openid" ident []
        attempt onFailure onSuccess res
