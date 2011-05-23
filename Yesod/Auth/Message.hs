{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Message
    ( AuthMessage (..)
    , defaultMessage
    ) where

import Text.Blaze (Html, toHtml)
import Data.Monoid (mappend)
import Data.Text (Text)

data AuthMessage =
      NoOpenID
    | LoginOpenID
    | Email
    | Password
    | Register
    | RegisterLong
    | EnterEmail
    | ConfirmationEmailSentTitle
    | ConfirmationEmailSent Text
    | AddressVerified
    | InvalidKeyTitle
    | InvalidKey
    | InvalidEmailPass
    | BadSetPass
    | SetPassTitle
    | SetPass
    | NewPass
    | ConfirmPass
    | PassMismatch
    | PassUpdated
    | Facebook
    | LoginViaEmail
    | InvalidLogin
    | NowLoggedIn
    | LoginTitle

defaultMessage :: AuthMessage -> Text
defaultMessage NoOpenID = "No OpenID identifier found"
defaultMessage LoginOpenID = "Login via OpenID"
defaultMessage Email = "Email"
defaultMessage Password = "Password"
defaultMessage Register = "Register"
defaultMessage RegisterLong = "Register a new account"
defaultMessage EnterEmail = "Enter your e-mail address below, and a confirmation e-mail will be sent to you."
defaultMessage ConfirmationEmailSentTitle = "Confirmation e-mail sent"
defaultMessage (ConfirmationEmailSent email) =
    "A confirmation e-mail has been sent to " `mappend`
    email `mappend`
    "."
defaultMessage AddressVerified = "Address verified, please set a new password"
defaultMessage InvalidKeyTitle = "Invalid verification key"
defaultMessage InvalidKey = "I'm sorry, but that was an invalid verification key."
defaultMessage InvalidEmailPass = "Invalid email/password combination"
defaultMessage BadSetPass = "You must be logged in to set a password"
defaultMessage SetPassTitle = "Set password"
defaultMessage SetPass = "Set a new password"
defaultMessage NewPass = "New password"
defaultMessage ConfirmPass = "Confirm"
defaultMessage PassMismatch = "Passwords did not match, please try again"
defaultMessage PassUpdated = "Password updated"
defaultMessage Facebook = "Login with Facebook"
defaultMessage LoginViaEmail = "Login via email"
defaultMessage InvalidLogin = "Invalid login"
defaultMessage NowLoggedIn = "You are now logged in"
defaultMessage LoginTitle = "Login"
