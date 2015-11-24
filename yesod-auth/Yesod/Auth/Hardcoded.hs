{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module      : Yesod.Auth.Hardcoded
Description : Very simple auth plugin for harcoded auth pairs.
Copyright   : (c) Arthur Fayzrakhmanov, 2015
License     : MIT
Maintainer  : heraldhoi@gmail.com
Stability   : experimental

Sometimes you may want to have some hardcoded set of users (e.g. site managers)
that allowed to log in and visit some specific sections of your website without
ability to register new managers.  This simple plugin is designed exactly for
this purpose.

Here is a quick example usage instruction.

= Enable plugin
First of all, add plugin to 'authPlugins' list:

@
instance YesodAuth App where
  authPlugins _ = [authHardcoded]
@

= Define a manager data type

@
data SiteManager = SiteManager
  { manUserName :: Text
  , manPassWord :: Text }

siteManagers :: [SiteManager]
siteManagers = [SiteManager "content editor" "top secret"]
@

= Describe a plugin instance of your app

@
instance YesodAuthHardcoded App where
  validatePassword u = return . validPassword u
  isUserNameExists   = return . lookupUser

lookupUser :: Text -> Bool
lookupUser username =
  case find (\m -> manUserName m == username) siteManagers of
    Just _ -> True
    _      -> False

validPassword :: Text -> Text -> Bool
validPassword u p =
  case find (\m -> manUserName m == u && manPassWord m == p) siteManagers of
    Just _ -> True
    _      -> False
@

= One caveat: 'authenticate' action of 'YesodAuth'.

You may want to store additional information for harcoded users in database, but
in this example let's cheat a bit:

@
instance YesodAuth App where
  authenticate _ =
    return (Authenticated (toSqlKey 0))
@

It is also possible to make 'authenticate' action smart enough to examine which
plugin was used to log in user, e.g.

@
authenticate creds =
  case credsPlugin creds of
    "hardcoded" -> -- ...
    -- ...
@
-}
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
