{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Message
    ( AuthMessage (..)
    , defaultMessage

      -- * All languages
    , englishMessage
    , portugueseMessage
    , swedishMessage
    , germanMessage
    , frenchMessage
    , norwegianBokmålMessage
    , japaneseMessage
    , finnishMessage
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
    | PleaseProvideUsername
    | PleaseProvidePassword

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
englishMessage PleaseProvideUsername = "Please fill in your username"
englishMessage PleaseProvidePassword = "Please fill in your password"

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
portugueseMessage PleaseProvideUsername = "Por favor digite seu nome de usuário"
portugueseMessage PleaseProvidePassword = "Por favor digite sua senha"

swedishMessage :: AuthMessage -> Text
swedishMessage NoOpenID = "Fann ej OpenID identifierare"
swedishMessage LoginOpenID = "Logga in via OpenID"
swedishMessage LoginGoogle = "Logga in via Google"
swedishMessage LoginYahoo = "Logga in via Yahoo"
swedishMessage Email = "Epost"
swedishMessage Password = "Lösenord"
swedishMessage Register = "Registrera"
swedishMessage RegisterLong = "Registrera ett nytt konto"
swedishMessage EnterEmail = "Skriv in din epost nedan så kommer ett konfirmationsmail skickas till adressen."
swedishMessage ConfirmationEmailSentTitle = "Konfirmationsmail skickat"
swedishMessage (ConfirmationEmailSent email) =
    "Ett konfirmationsmeddelande har skickats till" `mappend`
    email `mappend`
    "."
swedishMessage AddressVerified = "Adress verifierad, vänligen välj nytt lösenord"
swedishMessage InvalidKeyTitle = "Ogiltig verifikationsnyckel"
swedishMessage InvalidKey = "Tyvärr, du angav en ogiltig verifimationsnyckel."
swedishMessage InvalidEmailPass = "Ogiltig epost/lösenord kombination"
swedishMessage BadSetPass = "Du måste vara inloggad för att ange ett lösenord"
swedishMessage SetPassTitle = "Ange lösenord"
swedishMessage SetPass = "Ange nytt lösenord"
swedishMessage NewPass = "Nytt lösenord"
swedishMessage ConfirmPass = "Godkänn"
swedishMessage PassMismatch = "Lösenorden matcha ej, vänligen försök igen"
swedishMessage PassUpdated = "Lösenord updaterades"
swedishMessage Facebook = "Logga in med Facebook"
swedishMessage LoginViaEmail = "Logga in via epost"
swedishMessage InvalidLogin = "Ogiltigt login"
swedishMessage NowLoggedIn = "Du är nu inloggad"
swedishMessage LoginTitle = "Logga in"
swedishMessage PleaseProvideUsername = "Vänligen fyll i användarnamn"
swedishMessage PleaseProvidePassword = "Vänligen fyll i lösenord"

germanMessage :: AuthMessage -> Text
germanMessage NoOpenID = "Kein OpenID-Identifier gefunden"
germanMessage LoginOpenID = "Login via OpenID"
germanMessage LoginGoogle = "Login via Google"
germanMessage LoginYahoo = "Login via Yahoo"
germanMessage Email = "Email"
germanMessage Password = "Passwort"
germanMessage Register = "Registrieren"
germanMessage RegisterLong = "Neuen Account registrieren"
germanMessage EnterEmail = "Bitte die e-Mail Adresse angeben, eine Bestätigungsmail wird verschickt."
germanMessage ConfirmationEmailSentTitle = "Bestätigung verschickt."
germanMessage (ConfirmationEmailSent email) =
    "Eine Bestätigung wurde an " `mappend`
    email `mappend`
    "versandt."
germanMessage AddressVerified = "Adresse bestätigt, bitte neues Passwort angeben"
germanMessage InvalidKeyTitle = "Ungültiger Bestätigungsschlüssel"
germanMessage InvalidKey = "Das war leider ein ungültiger Bestätigungsschlüssel"
germanMessage InvalidEmailPass = "Ungültiger Nutzername oder Passwort"
germanMessage BadSetPass = "Um das Passwort zu ändern muss man eingeloggt sein"
germanMessage SetPassTitle = "Passwort angeben"
germanMessage SetPass = "Neues Passwort angeben"
germanMessage NewPass = "Neues Passwort"
germanMessage ConfirmPass = "Bestätigen"
germanMessage PassMismatch = "Die Passwörter stimmten nicht überein"
germanMessage PassUpdated = "Passwort überschrieben"
germanMessage Facebook = "Login über Facebook"
germanMessage LoginViaEmail = "Login via e-Mail"
germanMessage InvalidLogin = "Ungültiger Login"
germanMessage NowLoggedIn = "Login erfolgreich"
germanMessage LoginTitle = "Login"
germanMessage PleaseProvideUsername = "Bitte Nutzername angeben"
germanMessage PleaseProvidePassword = "Bitte Passwort angeben"



frenchMessage :: AuthMessage -> Text
frenchMessage NoOpenID = "Aucun fournisseur OpenID n'a été trouvé"
frenchMessage LoginOpenID = "Se connecter avec OpenID"
frenchMessage LoginGoogle = "Se connecter avec Google"
frenchMessage LoginYahoo = "Se connecter avec Yahoo"
frenchMessage Email = "Adresse électronique"
frenchMessage Password = "Mot de passe"
frenchMessage Register = "S'inscrire"
frenchMessage RegisterLong = "Créer un compte"
frenchMessage EnterEmail = "Entrez ci-dessous votre adresse électronique, et un message de confirmation vous sera envoyé"
frenchMessage ConfirmationEmailSentTitle = "Message de confirmation"
frenchMessage (ConfirmationEmailSent email) =
    "Un message de confirmation a été envoyé à " `mappend`
    email `mappend`
    "."
frenchMessage AddressVerified = "Votre adresse électronique a été validée, merci de choisir un nouveau mot de passe."
frenchMessage InvalidKeyTitle = "Clef de validation incorrecte"
frenchMessage InvalidKey = "Désolé, mais cette clef de validation est incorrecte"
frenchMessage InvalidEmailPass = "Le couple mot de passe/adresse électronique n'est pas correct"
frenchMessage BadSetPass = "Vous devez être connecté pour choisir un mot de passe"
frenchMessage SetPassTitle = "Changer de mot de passe"
frenchMessage SetPass = "Choisir un nouveau mot de passe"
frenchMessage NewPass = "Nouveau mot de passe"
frenchMessage ConfirmPass = "Confirmation du mot de passe"
frenchMessage PassMismatch = "Le deux mots de passe sont différents, veuillez les corriger"
frenchMessage PassUpdated = "Le mot de passe a bien été changé"
frenchMessage Facebook = "Se connecter avec Facebook"
frenchMessage LoginViaEmail = "Se connecter à l'aide d'une adresse électronique"
frenchMessage InvalidLogin = "Nom d'utilisateur incorrect"
frenchMessage NowLoggedIn = "Vous êtes maintenant connecté"
frenchMessage LoginTitle = "Se connecter"
frenchMessage PleaseProvideUsername = "Merci de renseigner votre nom d'utilisateur"
frenchMessage PleaseProvidePassword = "Merci de spécifier un mot de passe"

norwegianBokmålMessage :: AuthMessage -> Text
norwegianBokmålMessage NoOpenID = "Ingen OpenID-identifiserer funnet"
norwegianBokmålMessage LoginOpenID = "Logg inn med OpenID"
norwegianBokmålMessage LoginGoogle = "Logg inn med Google"
norwegianBokmålMessage LoginYahoo = "Logg inn med Yahoo"
norwegianBokmålMessage Email = "E-post"
norwegianBokmålMessage Password = "Passord"
norwegianBokmålMessage Register = "Registrer"
norwegianBokmålMessage RegisterLong = "Registrer en ny konto"
norwegianBokmålMessage EnterEmail = "Skriv inn e-postadressen din nedenfor og en e-postkonfirmasjon vil bli sendt."
norwegianBokmålMessage ConfirmationEmailSentTitle = "E-postkonfirmasjon sendt."
norwegianBokmålMessage (ConfirmationEmailSent email) =
    "En e-postkonfirmasjon har blitt sendt til " `mappend`
    email `mappend`
    "."
norwegianBokmålMessage AddressVerified = "Adresse verifisert, vennligst sett et nytt passord."
norwegianBokmålMessage InvalidKeyTitle = "Ugyldig verifiseringsnøkkel"
norwegianBokmålMessage InvalidKey = "Beklager, men det var en ugyldig verifiseringsnøkkel."
norwegianBokmålMessage InvalidEmailPass = "Ugyldig e-post/passord-kombinasjon"
norwegianBokmålMessage BadSetPass = "Du må være logget inn for å sette et passord."
norwegianBokmålMessage SetPassTitle = "Sett passord"
norwegianBokmålMessage SetPass = "Sett et nytt passord"
norwegianBokmålMessage NewPass = "Nytt passord"
norwegianBokmålMessage ConfirmPass = "Bekreft"
norwegianBokmålMessage PassMismatch = "Passordene stemte ikke overens, vennligst prøv igjen"
norwegianBokmålMessage PassUpdated = "Passord oppdatert"
norwegianBokmålMessage Facebook = "Logg inn med Facebook"
norwegianBokmålMessage LoginViaEmail = "Logg inn med e-post"
norwegianBokmålMessage InvalidLogin = "Ugyldig innlogging"
norwegianBokmålMessage NowLoggedIn = "Du er nå logget inn"
norwegianBokmålMessage LoginTitle = "Logg inn"
norwegianBokmålMessage PleaseProvideUsername = "Vennligst fyll inn ditt brukernavn"
norwegianBokmålMessage PleaseProvidePassword = "Vennligst fyll inn ditt passord"

japaneseMessage :: AuthMessage -> Text
japaneseMessage NoOpenID = "OpenID識別子がありません"
japaneseMessage LoginOpenID = "OpenIDでログイン"
japaneseMessage LoginGoogle = "Googleでログイン"
japaneseMessage LoginYahoo = "Yahooでログイン"
japaneseMessage Email = "Eメール"
japaneseMessage Password = "パスワード"
japaneseMessage Register = "登録"
japaneseMessage RegisterLong = "新規アカウント登録"
japaneseMessage EnterEmail = "メールアドレスを入力してください。確認メールが送られます"
japaneseMessage ConfirmationEmailSentTitle = "確認メールを送信しました"
japaneseMessage (ConfirmationEmailSent email) =
    "確認メールを " `mappend`
    email `mappend`
    " に送信しました"
japaneseMessage AddressVerified = "アドレスは認証されました。新しいパスワードを設定してください"
japaneseMessage InvalidKeyTitle = "認証キーが無効です"
japaneseMessage InvalidKey = "申し訳ありません。無効な認証キーです"
japaneseMessage InvalidEmailPass = "メールアドレスまたはパスワードが無効です"
japaneseMessage BadSetPass = "パスワードを設定するためには、ログインしてください"
japaneseMessage SetPassTitle = "パスワードの設定"
japaneseMessage SetPass = "新しいパスワードを設定する"
japaneseMessage NewPass = "新しいパスワード"
japaneseMessage ConfirmPass = "確認"
japaneseMessage PassMismatch = "パスワードが合いません。もう一度試してください"
japaneseMessage PassUpdated = "パスワードは更新されました"
japaneseMessage Facebook = "Facebookでログイン"
japaneseMessage LoginViaEmail = "Eメールでログイン"
japaneseMessage InvalidLogin = "無効なログインです"
japaneseMessage NowLoggedIn = "ログインしました"
japaneseMessage LoginTitle = "ログイン"
japaneseMessage PleaseProvideUsername = "ユーザ名を入力してください"
japaneseMessage PleaseProvidePassword = "パスワードを入力してください"

finnishMessage :: AuthMessage -> Text
finnishMessage NoOpenID = "OpenID-tunnistetta ei löydy"
finnishMessage LoginOpenID = "Kirjaudu OpenID-tilillä"
finnishMessage LoginGoogle = "Kirjaudu Google-tilillä"
finnishMessage LoginYahoo = "Kirjaudu Yahoo-tilillä"
finnishMessage Email = "Sähköposti"
finnishMessage Password = "Salasana"
finnishMessage Register = "Luo uusi"
finnishMessage RegisterLong = "Luo uusi tili"
finnishMessage EnterEmail = "Kirjoita alle sähköpostiosoitteesi, johon vahvistussähköposti lähetetään."
finnishMessage ConfirmationEmailSentTitle = "Vahvistussähköposti lähetetty."
finnishMessage (ConfirmationEmailSent email) =
    "Vahvistussähköposti on lähetty osoitteeseen " `mappend`
    email `mappend`
    "."
finnishMessage AddressVerified = "Sähköpostiosoite vahvistettu. Anna uusi salasana"
finnishMessage InvalidKeyTitle = "Virheellinen varmistusavain"
finnishMessage InvalidKey = "Valitettavasti varmistusavain on virheellinen."
finnishMessage InvalidEmailPass = "Virheellinen sähköposti tai salasana."
finnishMessage BadSetPass = "Kirjaudu ensin sisään asettaaksesi salasanan"
finnishMessage SetPassTitle = "Salasanan asettaminen"
finnishMessage SetPass = "Aseta uusi salasana"
finnishMessage NewPass = "Uusi salasana"
finnishMessage ConfirmPass = "Vahvista"
finnishMessage PassMismatch = "Salasanat eivät täsmää"
finnishMessage PassUpdated = "Salasana vaihdettu"
finnishMessage Facebook = "Kirjaudu Facebook-tilillä"
finnishMessage LoginViaEmail = "Kirjaudu sähköpostitilillä"
finnishMessage InvalidLogin = "Kirjautuminen epäonnistui"
finnishMessage NowLoggedIn = "Olet nyt kirjautunut sisään"
finnishMessage LoginTitle = "Kirjautuminen"
finnishMessage PleaseProvideUsername = "Käyttäjänimi puuttuu"
finnishMessage PleaseProvidePassword = "Salasana puuttuu"


