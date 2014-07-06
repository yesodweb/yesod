{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides a dummy authentication module that simply lets a user specify
-- his/her identifier. This is not intended for real world use, just for
-- testing.
module Yesod.Auth.Dummy
    ( authDummy
    ) where

import Yesod.Auth
import Yesod.Form (runInputPost, textField, ireq)
import Text.Hamlet (hamlet)
import Yesod.Core

authDummy :: YesodAuth m => AuthPlugin m
authDummy =
    AuthPlugin "dummy" dispatch login
  where
    dispatch "POST" [] = do
        ident <- lift $ runInputPost $ ireq textField "ident"
        lift $ setCredsRedirect $ Creds "dummy" ident []
    dispatch _ _ = notFound
    url = PluginR "dummy" []
    login authToMaster =
        toWidget [hamlet|
$newline never
<form method="post" action="@{authToMaster url}">
    Your new identifier is: #
    <input type="text" name="ident">
    <input type="submit" value="Dummy Login">
|]
