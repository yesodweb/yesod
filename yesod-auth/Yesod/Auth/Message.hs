{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Message
    ( AuthMessage (..)
    , defaultMessage

      -- * All languages
    , englishMessage
    , portugueseMessage
    ) where

import Data.Monoid (mappend)
import Data.Text (Text)

data AuthMessage =
      NoOpenID
    | LoginOpenID
    | LoginGoogle
    | LoginYahoo
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

-- | Defaults to 'englishMessage'.
defaultMessage :: AuthMessage -> Text
defaultMessage = englishMessage

englishMessage :: AuthMessage -> Text
englishMessage NoOpenID = "No OpenID identifier found"
englishMessage LoginOpenID = "Login via OpenID"
englishMessage LoginGoogle = "Login via Google"
englishMessage LoginYahoo = "Login via Yahoo"
englishMessage Email = "Email"
englishMessage Password = "Password"
englishMessage Register = "Register"
englishMessage RegisterLong = "Register a new account"
englishMessage EnterEmail = "Enter your e-mail address below, and a confirmation e-mail will be sent to you."
englishMessage ConfirmationEmailSentTitle = "Confirmation e-mail sent"
englishMessage (ConfirmationEmailSent email) =
    "A confirmation e-mail has been sent to " `mappend`
    email `mappend`
    "."
englishMessage AddressVerified = "Address verified, please set a new password"
englishMessage InvalidKeyTitle = "Invalid verification key"
englishMessage InvalidKey = "I'm sorry, but that was an invalid verification key."
englishMessage InvalidEmailPass = "Invalid email/password combination"
englishMessage BadSetPass = "You must be logged in to set a password"
englishMessage SetPassTitle = "Set password"
englishMessage SetPass = "Set a new password"
englishMessage NewPass = "New password"
englishMessage ConfirmPass = "Confirm"
englishMessage PassMismatch = "Passwords did not match, please try again"
englishMessage PassUpdated = "Password updated"
englishMessage Facebook = "Login with Facebook"
englishMessage LoginViaEmail = "Login via email"
englishMessage InvalidLogin = "Invalid login"
englishMessage NowLoggedIn = "You are now logged in"
englishMessage LoginTitle = "Login"

portugueseMessage :: AuthMessage -> Text
portugueseMessage NoOpenID = "Nenhum identificador OpenID encontrado"
portugueseMessage LoginOpenID = "Entrar via OpenID"
portugueseMessage LoginGoogle = "Entrar via Google"
portugueseMessage LoginYahoo = "Entrar via Yahoo"
portugueseMessage Email = "E-mail"
portugueseMessage Password = "Senha"
portugueseMessage Register = "Registrar"
portugueseMessage RegisterLong = "Registrar uma nova conta"
portugueseMessage EnterEmail = "Por favor digite seu endereço de e-mail abaixo e um e-mail de confirmação será enviado para você."
portugueseMessage ConfirmationEmailSentTitle = "E-mail de confirmação enviado"
portugueseMessage (ConfirmationEmailSent email) =
    "Um e-mail de confirmação foi enviado para " `mappend`
    email `mappend`
    "."
portugueseMessage AddressVerified = "Endereço verificado, por favor entre com uma nova senha"
portugueseMessage InvalidKeyTitle = "Chave de verificação inválida"
portugueseMessage InvalidKey = "Por favor nos desculpe, mas essa é uma chave de verificação inválida."
portugueseMessage InvalidEmailPass = "E-mail e/ou senha inválidos"
portugueseMessage BadSetPass = "Você deve entrar para definir uma senha"
portugueseMessage SetPassTitle = "Definir senha"
portugueseMessage SetPass = "Definir uma nova senha"
portugueseMessage NewPass = "Nova senha"
portugueseMessage ConfirmPass = "Confirmar"
portugueseMessage PassMismatch = "Senhas não conferem, por favor tente novamente"
portugueseMessage PassUpdated = "Senhas alteradas"
portugueseMessage Facebook = "Entrar via Facebook"
portugueseMessage LoginViaEmail = "Entrar via e-mail"
portugueseMessage InvalidLogin = "Informações de login inválidas"
portugueseMessage NowLoggedIn = "Você acaba de entrar no site com sucesso!"
portugueseMessage LoginTitle = "Entrar no site"
