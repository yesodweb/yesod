{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Kerberos
    ( authKerberos,
      genericAuthKerberos,
      KerberosConfig(..)
    ) where

#include "qq.h"

import Yesod.Auth
import Web.Authenticate.Kerberos
import Data.Text (Text)
import qualified Data.Text as T
import Text.Hamlet
import Yesod.Handler
import Yesod.Widget
import Control.Monad.IO.Class (liftIO)
import Yesod.Form
import Control.Applicative ((<$>), (<*>))

data KerberosConfig = KerberosConfig {
    -- | When a user gives username x, f(x) will be passed to Kerberos
    usernameModifier :: Text -> Text
    -- | When a user gives username x, f(x) will be passed to Yesod
  , identifierModifier :: Text -> Text
  }

defaultKerberosConfig :: KerberosConfig
defaultKerberosConfig = KerberosConfig id id

genericAuthKerberos :: YesodAuth m => KerberosConfig -> AuthPlugin m
genericAuthKerberos config = AuthPlugin "kerberos" dispatch $ \tm -> addHamlet
    [hamlet|
    <div id="header">
        <h1>Login

    <div id="login">
        <form method="post" action="@{tm login}">
            <table>
                <tr>
                    <th>Username:
                    <td>
                        <input id="x" name="username" autofocus="" required>
                <tr>
                    <th>Password:
                    <td>
                        <input type="password" name="password" required>
                <tr>
                    <td>&nbsp;
                    <td>
                        <input type="submit" value="Login">

            <script>
                if (!("autofocus" in document.createElement("input"))) {
                    document.getElementById("x").focus();
                }
|]
  where
    dispatch "POST" ["login"] = postLoginR config >>= sendResponse
    dispatch _ _              = notFound

login :: AuthRoute
login = PluginR "kerberos" ["login"]

authKerberos :: YesodAuth m => AuthPlugin m
authKerberos = genericAuthKerberos defaultKerberosConfig

-- | Handle the login form
postLoginR :: (YesodAuth y) => KerberosConfig -> GHandler Auth y ()
postLoginR config = do
    (mu,mp) <- runInputPost $ (,)
        <$> iopt textField "username"
        <*> iopt textField "password"

    let errorMessage (message :: Text) = do
        setMessage [html| Error: #{message} |]
        toMaster <- getRouteToMaster
        redirect RedirectTemporary $ toMaster LoginR

    case (mu,mp) of
        (Nothing, _      ) -> errorMessage "Please fill in your username"
        (_      , Nothing) -> errorMessage "Please fill in your password"
        (Just u , Just p ) -> do
          result <- liftIO $ loginKerberos (usernameModifier config u) p
          case result of
            Ok -> do
                let creds = Creds
                      { credsIdent  = identifierModifier config u
                      , credsPlugin = "Kerberos"
                      , credsExtra  = []
                      }
                setCreds True creds
            kerberosError -> errorMessage (T.pack $ show kerberosError)

