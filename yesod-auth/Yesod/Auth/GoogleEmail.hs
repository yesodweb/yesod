{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
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

import Yesod.Core
import Data.Text (Text)
import qualified Yesod.Auth.Message as Msg
import qualified Data.Text as T
import Control.Exception.Lifted (try, SomeException)

pid :: Text
pid = "googleemail"

forwardUrl :: AuthRoute
forwardUrl = PluginR pid ["forward"]

googleIdent :: Text
googleIdent = "https://www.google.com/accounts/o8/id"

authGoogleEmail :: YesodAuth m => AuthPlugin m
authGoogleEmail =
    AuthPlugin pid dispatch login
  where
    complete = PluginR pid ["complete"]
    login tm =
        [whamlet|<a href=@{tm forwardUrl}>_{Msg.LoginGoogle}|]
    dispatch "GET" ["forward"] = do
        render <- getUrlRender
        let complete' = render complete
        master <- lift getYesod
        eres <- lift $ try $ OpenId.getForwardUrl googleIdent complete' Nothing
            [ ("openid.ax.type.email", "http://schema.openid.net/contact/email")
            , ("openid.ns.ax", "http://openid.net/srv/ax/1.0")
            , ("openid.ns.ax.required", "email")
            , ("openid.ax.mode", "fetch_request")
            , ("openid.ax.required", "email")
            , ("openid.ui.icon", "true")
            ] (authHttpManager master)
        either
          (\err -> do
            tm <- getRouteToParent
            lift $ loginErrorMessage (tm LoginR) $ T.pack $ show (err :: SomeException))
          redirect
          eres
    dispatch "GET" ["complete", ""] = dispatch "GET" ["complete"] -- compatibility issues
    dispatch "GET" ["complete"] = do
        rr <- getRequest
        completeHelper $ reqGetParams rr
    dispatch "POST" ["complete", ""] = dispatch "POST" ["complete"] -- compatibility issues
    dispatch "POST" ["complete"] = do
        (posts, _) <- runRequestBody
        completeHelper posts
    dispatch _ _ = notFound

completeHelper :: YesodAuth master => [(Text, Text)] -> AuthHandler master TypedContent
completeHelper gets' = do
      master <- lift getYesod
      eres <- lift $ try $ OpenId.authenticateClaimed gets' (authHttpManager master)
      tm <- getRouteToParent
      either (onFailure tm) (onSuccess tm) eres
    where
      onFailure tm err =
        lift $ loginErrorMessage (tm LoginR) $ T.pack $ show (err :: SomeException)
      onSuccess tm oir = do
              let OpenId.Identifier ident = OpenId.oirOpLocal oir
              memail <- lookupGetParam "openid.ext1.value.email"
              case (memail, "https://www.google.com/accounts/o8/id" `T.isPrefixOf` ident) of
                  (Just email, True) -> lift $ setCredsRedirect $ Creds pid email []
                  (_, False)   -> lift $ loginErrorMessage (tm LoginR) "Only Google login is supported"
                  (Nothing, _) -> lift $ loginErrorMessage (tm LoginR) "No email address provided"
