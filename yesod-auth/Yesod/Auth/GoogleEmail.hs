{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Use an email address as an identifier via Google's OpenID login system.
--
-- This backend will not use the OpenID identifier at all. It only uses OpenID
-- as a login system. By using this plugin, you are trusting Google to validate
-- an email address, and requiring users to have a Google account. On the plus
-- side, you get to use email addresses as the identifier, many users have
-- existing Google accounts, the login system has been long tested (as opposed
-- to BrowserID), and it requires no credential managing or setup (as opposed
-- to Email).
module Yesod.Auth.GoogleEmail
    ( authGoogleEmail
    , forwardUrl
    ) where

import Yesod.Auth
import qualified Web.Authenticate.OpenId as OpenId
import Control.Monad.Attempt

import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Yesod.Request
import Text.Blaze (toHtml)
import Data.Text (Text)
import qualified Yesod.Auth.Message as Msg
import qualified Data.Text as T

forwardUrl :: AuthRoute
forwardUrl = PluginR "googleemail" ["forward"]

authGoogleEmail :: YesodAuth m => AuthPlugin m
authGoogleEmail =
    AuthPlugin "googleemail" dispatch login
  where
    complete = PluginR "googleemail" ["complete"]
    name = "openid_identifier"
    login tm = do
        [whamlet|
<form method=get action=@{tm forwardUrl}>
    <input type=hidden name=openid_identifier value=https://www.google.com/accounts/o8/id>
    <input type=submit value=_{Msg.LoginTitle}>
|]
    dispatch "GET" ["forward"] = do
        roid <- runInputGet $ iopt textField name
        case roid of
            Just oid -> do
                render <- getUrlRender
                toMaster <- getRouteToMaster
                let complete' = render $ toMaster complete
                res <- runAttemptT $ OpenId.getForwardUrl oid complete' Nothing
                    [ ("openid.ax.type.email", "http://schema.openid.net/contact/email")
                    , ("openid.ns.ax", "http://openid.net/srv/ax/1.0")
                    , ("openid.ns.ax.required", "email")
                    , ("openid.ax.mode", "fetch_request")
                    , ("openid.ax.required", "email")
                    , ("openid.ui.icon", "true")
                    ]
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
        let onSuccess (OpenId.Identifier ident, _) = do
                memail <- lookupGetParam "openid.ext1.value.email"
                case (memail, "https://www.google.com/accounts/o8/id" `T.isPrefixOf` ident) of
                    (Just email, True) -> setCreds True $ Creds "openid" email []
                    (_, False) -> do
                        setMessage "Only Google login is supported"
                        redirect RedirectTemporary $ toMaster LoginR
                    (Nothing, _) -> do
                        setMessage "No email address provided"
                        redirect RedirectTemporary $ toMaster LoginR
        attempt onFailure onSuccess res
