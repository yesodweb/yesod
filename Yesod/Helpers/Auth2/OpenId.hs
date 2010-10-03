{-# LANGUAGE QuasiQuotes #-}
module Yesod.Helpers.Auth2.OpenId
    ( authOpenId
    ) where

import Yesod
import Yesod.Helpers.Auth2
import qualified Web.Authenticate.OpenId as OpenId
import Control.Monad.Attempt
import Network.OpenID

authOpenId :: YesodAuth m => AuthPlugin m
authOpenId =
    AuthPlugin "openid" dispatch login
  where
    forward = PluginR "openid" ["forward"]
    complete1 = PluginR "openid" ["complete1"]
    complete2 = PluginR "openid" ["complete2"]
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
    forward2 complete' oid = do
        case normalizeIdentifier $ Identifier oid of
          Nothing -> return $ "Unable to normalize identifier: " ++ oid
          Just ident -> do
            let resolve = liftIO . makeRequest True
            rpi <- liftIO $ discover resolve ident
            case rpi of
              Left err -> return $ "Error on discovery: " ++ show err
              Right (p, i) -> do
                eam <- liftIO $ associate emptyAssociationMap True resolve p
                case eam of
                  Left err -> return $ "Error on associate: " ++ show err
                  Right am -> do
                    let au = authenticationURI am Setup p i complete' Nothing
                    setSession "OPENID_AM" $ show am
                    redirectString RedirectTemporary $ show au
    dispatch "POST" ["forward"] = do
        (roid, _, _) <- runFormPost $ stringInput name
        case roid of
            FormSuccess oid -> do
                render <- getUrlRender
                toMaster <- getRouteToMaster
                let complete2' = render $ toMaster complete2
                msg <- forward2 complete2' oid
                let complete' = render $ toMaster complete1
                res <- runAttemptT $ OpenId.getForwardUrl oid complete'
                attempt
                  (\err -> do
                        setMessage $ string $ unlines
                            [ show err
                            , msg
                            ]
                        redirect RedirectTemporary $ toMaster LoginR
                        )
                  (redirectString RedirectTemporary)
                  res
            _ -> do
                toMaster <- getRouteToMaster
                setMessage $ string "No OpenID identifier found"
                redirect RedirectTemporary $ toMaster LoginR
    dispatch "GET" ["complete1"] = do
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
    dispatch "GET" ["complete2"] = do
        amString <- lookupSession "OPENID_AM"
        deleteSession "OPENID_AM"
        params <- reqGetParams `fmap` getRequest
        let am = case amString >>= readMay of
                    Nothing -> emptyAssociationMap
                    Just x -> x
        let resolve = liftIO . makeRequest True
        render <- getUrlRender
        toMaster <- getRouteToMaster
        let complete2' = render $ toMaster complete2
        res <- liftIO $ verifyAuthentication am params complete2' resolve
        let mident = lookup "openid.identity" params
        case (res, mident) of
            (Right (), Just ident) ->
                setCreds True $ Creds "openid" ident []
            _ -> do
                setMessage $ string "Error logging in via OpenID"
                redirect RedirectTemporary $ toMaster LoginR
    dispatch _ _ = notFound

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing
