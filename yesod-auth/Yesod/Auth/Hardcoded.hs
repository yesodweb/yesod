{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module      : Yesod.Auth.Hardcoded
Description : Very simple auth plugin for hardcoded auth pairs.
Copyright   : (c) Arthur Fayzrakhmanov, 2015
License     : MIT
Maintainer  : heraldhoi@gmail.com
Stability   : experimental

Sometimes you may want to have some hardcoded set of users (e.g. site managers)
that allowed to log in and visit some specific sections of your website without
ability to register new managers.  This simple plugin is designed exactly for
this purpose.

Here is a quick usage example.

== Define hardcoded users representation

Let's assume, that we want to have some hardcoded managers with normal site
users.  Let's define hardcoded user representation:

@
data SiteManager = SiteManager
  { manUserName :: Text
  , manPassWord :: Text }
  deriving Show

siteManagers :: [SiteManager]
siteManagers = [SiteManager "content editor" "top secret"]
@


== Describe 'YesodAuth' instance

Now we need to have some convenient 'AuthId' type representing both
cases:

@
instance YesodAuth App where
  type AuthId App = Either UserId Text
@

Here, right @Text@ value will present hardcoded user name (which obviously must
be unique).

'AuthId' must have an instance of 'PathPiece' class, this is needed to store
user identifier in session (this happens in 'setCreds' and 'setCredsRedirect'
actions) and to read that identifier from session (this happens in
`dafaultMaybeAuthId` action).  So we have to define it:

@
import Text.Read (readMaybe)

instance PathPiece (Either UserId Text) where
  fromPathPiece = readMaybe . unpack
  toPathPiece = pack . show
@

Quiet simple so far.  Now let's add plugin to 'authPlugins' list, and define
'authenticate' method, it should return user identifier for given credentials,
for normal users it is usually persistent key, for hardcoded users we will
return user name again.

@
instance YesodAuth App where
  -- ..
  authPlugins _ = [authHardcoded]

  authenticate Creds{..} =
    return
      (case credsPlugin of
         "hardcoded" ->
           case lookupUser credsIdent of
             Nothing -> UserError InvalidLogin
             Just m  -> Authenticated (Right (manUserName m)))
@

Here @lookupUser@ is just a helper function to lookup hardcoded users by name:

@
lookupUser :: Text -> Maybe SiteManager
lookupUser username = find (\m -> manUserName m == username) siteManagers
@


== Describe an 'YesodAuthPersist' instance

Now we need to manually define 'YesodAuthPersist' instance.

> instance YesodAuthPersist App where
>   type AuthEntity App = Either User SiteManager
>
>   getAuthEntity (Left uid) =
>     do x <- runDB (get uid)
>        return (Left <$> x)
>   getAuthEntity (Right username) = return (Right <$> lookupUser username)


== Define 'YesodAuthHardcoded' instance

Finally, let's define an plugin instance

@
instance YesodAuthHardcoded App where
  validatePassword u = return . validPassword u
  doesUserNameExist  = return . isJust . lookupUser

validPassword :: Text -> Text -> Bool
validPassword u p =
  case find (\m -> manUserName m == u && manPassWord m == p) siteManagers of
    Just _ -> True
    _      -> False
@


== Conclusion

Now we can use 'maybeAuthId', 'maybeAuthPair', 'requireAuthId', and
'requireAuthPair', moreover, the returned value makes possible to distinguish
normal users and site managers.
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
    loginWidget toMaster = do
      request <- getRequest
      [whamlet|
        $newline never
        <form method="post" action="@{toMaster loginR}">
          $maybe t <- reqToken request
            <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
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
