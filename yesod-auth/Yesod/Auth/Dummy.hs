{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides a dummy authentication module that simply lets a user specify
-- his/her identifier. This is not intended for real world use, just for
-- testing.
module Yesod.Auth.Dummy
    ( authDummy
    ) where

#include "qq.h"

import Yesod.Auth
import Yesod.Form (runInputPost, textField, ireq)
import Yesod.Handler (notFound)
import Text.Hamlet (hamlet)
import Yesod.Widget (addHamlet)

authDummy :: YesodAuth m => AuthPlugin m
authDummy =
    AuthPlugin "dummy" dispatch login
  where
    dispatch "POST" [] = do
        ident <- runInputPost $ ireq textField "ident"
        setCreds True $ Creds "dummy" ident []
    dispatch _ _ = notFound
    url = PluginR "dummy" []
    login authToMaster =
        addHamlet [QQ(hamlet)|
<form method="post" action="@{authToMaster url}">
    \Your new identifier is: 
    <input type="text" name="ident">
    <input type="submit" value="Dummy Login">
|]
