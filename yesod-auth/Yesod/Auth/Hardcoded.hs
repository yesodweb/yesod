{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Yesod.Auth.Hardcoded
  ( YesodAuthHardcoded(..)
  , authHardcoded
  , loginR )
  where

import           Yesod.Auth          (Auth, AuthPlugin (..), AuthRoute,
                                      Creds (..), Route (..), YesodAuth,
                                      loginErrorMessageI, setCredsRedirect)
import qualified Yesod.Auth.Message  as Msg
import           Yesod.Core
import           Yesod.Form          (ireq, runInputPost, textField)

import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text)


loginR :: AuthRoute
loginR = PluginR "hardcoded" ["login"]

class (YesodAuth site) => YesodAuthHardcoded site where

  -- | Check whether given user name exists among hardcoded names.
  doesUserNameExist :: Text -> HandlerT site IO Bool

  -- | Validate given user name with given password.
  validatePassword :: Text -> Text -> HandlerT site IO Bool


authHardcoded :: YesodAuthHardcoded m => AuthPlugin m
authHardcoded =
  AuthPlugin "hardcoded" dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget toMaster =
      [whamlet|
        $newline never
        <form method="post" action="@{toMaster loginR}">
          <table>
            <tr>
              <th>_{Msg.UserName}
              <td>
                 <input type="text" name="username" required>
            <tr>
              <th>_{Msg.Password}
              <td>
                 <input type="password" name="password" required>
            <tr>
              <td colspan="2">
                 <button type="submit" .btn .btn-success>_{Msg.LoginTitle}
        |]


postLoginR :: (YesodAuthHardcoded master)
           => HandlerT Auth (HandlerT master IO) TypedContent
postLoginR =
  do (username, password) <- lift (runInputPost
       ((,) <$> ireq textField "username"
            <*> ireq textField "password"))
     isValid <- lift (validatePassword username password)
     if isValid
        then lift (setCredsRedirect (Creds "hardcoded" username []))
        else do isExists <- lift (doesUserNameExist username)
                loginErrorMessageI LoginR
                                   (if isExists
                                       then Msg.InvalidUsernamePass
                                       else Msg.IdentifierNotFound username)
