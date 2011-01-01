{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Yesod.Helpers.Auth.OpenId
    ( authOpenId
    , forwardUrl
    ) where

import Yesod.Helpers.Auth
import qualified Web.Authenticate.OpenId as OpenId
import Control.Monad.Attempt

import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Yesod.Request
import Text.Hamlet (hamlet)
import Text.Cassius (cassius)
import Text.Blaze (string)
import Control.Monad.IO.Class (liftIO)

forwardUrl :: AuthRoute
forwardUrl = PluginR "openid" ["forward"]

authOpenId :: YesodAuth m => AuthPlugin m
authOpenId =
    AuthPlugin "openid" dispatch login
  where
    complete = PluginR "openid" ["complete"]
    name = "openid_identifier"
    login tm = do
        ident <- newIdent
        addCassius
#if GHC7
            [cassius|
#else
            [$cassius|
#endif
  #$ident$
    background: #fff url(http://www.myopenid.com/static/openid-icon-small.gif) no-repeat scroll 0pt 50%;
    padding-left: 18px;
|]
        addHamlet
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
%form!method=get!action=@tm.forwardUrl@
    %label!for=$ident$ OpenID: $
    %input#$ident$!type=text!name=$name$!value="http://"
    %input!type=submit!value="Login via OpenID"
|]
    dispatch "GET" ["forward"] = do
        (roid, _, _) <- runFormGet $ stringInput name
        case roid of
            FormSuccess oid -> do
                render <- getUrlRender
                toMaster <- getRouteToMaster
                let complete' = render $ toMaster complete
                res <- runAttemptT $ OpenId.getForwardUrl oid complete' Nothing []
                attempt
                  (\err -> do
                        setMessage $ string $ show err
                        redirect RedirectTemporary $ toMaster LoginR
                        )
                  (redirectString RedirectTemporary)
                  res
            _ -> do
                toMaster <- getRouteToMaster
                setMessage $ string "No OpenID identifier found"
                redirect RedirectTemporary $ toMaster LoginR
    dispatch "GET" ["complete"] = do
        rr <- getRequest
        completeHelper $ reqGetParams rr
    dispatch "POST" ["complete"] = do
        rr <- getRequest
        (posts, _) <- liftIO $ reqRequestBody rr
        completeHelper posts
    dispatch _ _ = notFound

completeHelper :: YesodAuth m => [(String, String)] -> GHandler Auth m ()
completeHelper gets' = do
        res <- runAttemptT $ OpenId.authenticate gets'
        toMaster <- getRouteToMaster
        let onFailure err = do
            setMessage $ string $ show err
            redirect RedirectTemporary $ toMaster LoginR
        let onSuccess (OpenId.Identifier ident, _) =
                setCreds True $ Creds "openid" ident []
        attempt onFailure onSuccess res
