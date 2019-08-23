{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Provides a dummy authentication module that simply lets a user specify
-- their identifier. This is not intended for real world use, just for
-- testing. This plugin supports form submissions via JSON (since 1.6.8).
--
-- = Using the JSON Login Endpoint
--
-- We are assuming that you have declared `authRoute` as follows
--
-- @
--       Just $ AuthR LoginR
-- @
--
-- If you are using a different one, then you have to adjust the
-- endpoint accordingly.
--
-- @
--       Endpoint: \/auth\/page\/dummy
--       Method: POST
--       JSON Data: {
--                      "ident": "my identifier"
--                  }
-- @
--
-- Remember to add the following headers:
--
--     - Accept: application\/json
--     - Content-Type: application\/json

module Yesod.Auth.Dummy
    ( authDummy
    ) where

import Yesod.Auth
import Yesod.Form (runInputPost, textField, ireq)
import Yesod.Core
import Data.Text (Text)
import Data.Aeson.Types (Result(..), Parser)
import qualified Data.Aeson.Types as A (parseEither, withObject)

identParser :: Value -> Parser Text
identParser = A.withObject "Ident" (.: "ident")

authDummy :: YesodAuth m => AuthPlugin m
authDummy =
    AuthPlugin "dummy" dispatch login
  where
    dispatch "POST" [] = do
        (jsonResult :: Result Value) <- parseCheckJsonBody
        eIdent <- case jsonResult of
            Success val -> return $ A.parseEither identParser val
            Error   err -> return $ Left err
        case eIdent of
            Right ident ->
                setCredsRedirect $ Creds "dummy" ident []
            Left  _     -> do
                ident <- runInputPost $ ireq textField "ident"
                setCredsRedirect $ Creds "dummy" ident []
    dispatch _ _ = notFound
    url = PluginR "dummy" []
    login authToMaster = do
        request <- getRequest
        toWidget [hamlet|
$newline never
<form method="post" action="@{authToMaster url}">
    $maybe t <- reqToken request
        <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
    Your new identifier is: #
    <input type="text" name="ident">
    <input type="submit" value="Dummy Login">
|]
