{-# LANGUAGE QuasiQuotes #-}
-- | Provides a dummy authentication module that simply lets a user specify
-- his/her identifier. This is not intended for real world use, just for
-- testing.
module Yesod.Helpers.Auth.Dummy
    ( authDummy
    ) where

import Yesod
import Yesod.Helpers.Auth

authDummy :: YesodAuth m => AuthPlugin m
authDummy =
    AuthPlugin "dummy" dispatch login
  where
    dispatch "POST" [] = do
        ident <- runFormPost' $ stringInput "ident"
        setCreds True $ Creds "dummy" ident []
    dispatch _ _ = notFound
    url = PluginR "dummy" []
    login authToMaster = do
        addBody [$hamlet|
%form!method=post!action=@authToMaster.url@
    Your new identifier is: $
    %input!type=text!name=ident
    %input!type=submit!value="Dummy Login"
|]
