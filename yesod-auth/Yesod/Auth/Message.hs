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
    , chineseMessage
    , croatianMessage
    , spanishMessage
    , czechMessage
    , russianMessage
    , dutchMessage
    , danishMessage
    , koreanMessage
    ) where

import           Data.Monoid (mappend, (<>))
import           Data.Text   (Text)

data AuthMessage =
      NoOpenID
    | LoginOpenID
    | LoginGoogle
    | LoginYahoo
    | Email
    | UserName
    | IdentifierNotFound Text
    | Password
    | Register
    | RegisterLong
    | EnterEmail
    | ConfirmationEmailSentTitle
    | ConfirmationEmailSent Text
    | AddressVerified
    | EmailVerifiedChangePass
    | EmailVerified
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
    | NoIdentifierProvided
    | InvalidEmailAddress
    | PasswordResetTitle
    | ProvideIdentifier
    | SendPasswordResetEmail
    | PasswordResetPrompt
    | CurrentPassword
    | InvalidUsernamePass
    | Logout
    | LogoutTitle
    | AuthError
{-# DEPRECATED Logout "Please, use LogoutTitle instead." #-}
{-# DEPRECATED AddressVerified "Please, use EmailVerifiedChangePass instead." #-}

-- | Defaults to 'englishMessage'.
defaultMessage :: AuthMessage -> Text
defaultMessage = englishMessage

englishMessage :: AuthMessage -> Text
englishMessage NoOpenID = "No OpenID identifier found"
englishMessage LoginOpenID = "Log in via OpenID"
englishMessage LoginGoogle = "Log in via Google"
englishMessage LoginYahoo = "Log in via Yahoo"
englishMessage Email = "Email"
englishMessage UserName = "User name"
englishMessage Password = "Password"
englishMessage CurrentPassword = "Current Password"
englishMessage Register = "Register"
englishMessage RegisterLong = "Register a new account"
englishMessage EnterEmail = "Enter your e-mail address below, and a confirmation e-mail will be sent to you."
englishMessage ConfirmationEmailSentTitle = "Confirmation e-mail sent"
englishMessage (ConfirmationEmailSent email) =
    "A confirmation e-mail has been sent to " `Data.Monoid.mappend`
    email `mappend`
    "."
englishMessage AddressVerified = "Email address verified, please set a new password"
englishMessage EmailVerifiedChangePass = "Email address verified, please set a new password"
englishMessage EmailVerified = "Email address verified"
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
englishMessage Facebook = "Log in with Facebook"
englishMessage LoginViaEmail = "Log in via email"
englishMessage InvalidLogin = "Invalid login"
englishMessage NowLoggedIn = "You are now logged in"
englishMessage LoginTitle = "Log In"
englishMessage PleaseProvideUsername = "Please fill in your username"
englishMessage PleaseProvidePassword = "Please fill in your password"
englishMessage NoIdentifierProvided = "No email/username provided"
englishMessage InvalidEmailAddress = "Invalid email address provided"
englishMessage PasswordResetTitle = "Password Reset"
englishMessage ProvideIdentifier = "Email or Username"
englishMessage SendPasswordResetEmail = "Send password reset email"
englishMessage PasswordResetPrompt = "Enter your e-mail address or username below, and a password reset e-mail will be sent to you."
englishMessage InvalidUsernamePass = "Invalid username/password combination"
englishMessage (IdentifierNotFound ident) = "Login not found: " `mappend` ident
englishMessage Logout = "Log Out"
englishMessage LogoutTitle = "Log Out"
englishMessage AuthError = "Authentication Error" -- FIXME by Google Translate

portugueseMessage :: AuthMessage -> Text
portugueseMessage NoOpenID = "Nenhum identificador OpenID encontrado"
portugueseMessage LoginOpenID = "Entrar via OpenID"
portugueseMessage LoginGoogle = "Entrar via Google"
portugueseMessage LoginYahoo = "Entrar via Yahoo"
portugueseMessage Email = "E-mail"
portugueseMessage UserName = "Nome de usuário" -- FIXME by Google Translate "user name"
portugueseMessage Password = "Senha"
portugueseMessage CurrentPassword = "Palavra de passe"
portugueseMessage Register = "Registrar"
portugueseMessage RegisterLong = "Registrar uma nova conta"
portugueseMessage EnterEmail = "Por favor digite seu endereço de e-mail abaixo e um e-mail de confirmação será enviado para você."
portugueseMessage ConfirmationEmailSentTitle = "E-mail de confirmação enviado"
portugueseMessage (ConfirmationEmailSent email) =
    "Um e-mail de confirmação foi enviado para " `mappend`
    email `mappend`
    "."
portugueseMessage AddressVerified = "Endereço verificado, por favor entre com uma nova senha"
portugueseMessage EmailVerifiedChangePass = "Endereço verificado, por favor entre com uma nova senha"
portugueseMessage EmailVerified = "Endereço verificado"
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
portugueseMessage NoIdentifierProvided = "Nenhum e-mail ou nome de usuário informado"
portugueseMessage InvalidEmailAddress = "Endereço de e-mail inválido informado"
portugueseMessage PasswordResetTitle = "Resetar senha"
portugueseMessage ProvideIdentifier = "E-mail ou nome de usuário"
portugueseMessage SendPasswordResetEmail = "Enviar e-mail para resetar senha"
portugueseMessage PasswordResetPrompt = "Insira seu endereço de e-mail ou nome de usuário abaixo.  Um e-mail para resetar sua senha será enviado para você."
portugueseMessage InvalidUsernamePass = "Nome de usuário ou senha inválidos"
-- TODO
portugueseMessage i@(IdentifierNotFound _) = englishMessage i
portugueseMessage Logout = "Sair" -- FIXME by Google Translate
portugueseMessage LogoutTitle = "Sair" -- FIXME by Google Translate
portugueseMessage AuthError = "Erro de autenticação" -- FIXME by Google Translate

spanishMessage :: AuthMessage -> Text
spanishMessage NoOpenID = "No se encuentra el identificador OpenID"
spanishMessage LoginOpenID = "Entrar utilizando OpenID"
spanishMessage LoginGoogle = "Entrar utilizando Google"
spanishMessage LoginYahoo = "Entrar utilizando Yahoo"
spanishMessage Email = "Correo electrónico"
spanishMessage UserName = "Nombre de Usuario"
spanishMessage Password = "Contraseña"
spanishMessage CurrentPassword = "Contraseña actual"
spanishMessage Register = "Registrarse"
spanishMessage RegisterLong = "Registrar una nueva cuenta"
spanishMessage EnterEmail = "Coloque su dirección de correo electrónico, y un correo de confirmación le será enviado a su cuenta."
spanishMessage ConfirmationEmailSentTitle = "La confirmación de correo ha sido enviada"
spanishMessage (ConfirmationEmailSent email) =
    "Una confirmación de correo electrónico ha sido enviada a " `mappend`
    email `mappend`
    "."
spanishMessage AddressVerified = "Dirección verificada, por favor introduzca una contraseña"
spanishMessage EmailVerifiedChangePass = "Dirección verificada, por favor introduzca una contraseña"
spanishMessage EmailVerified = "Dirección verificada"
spanishMessage InvalidKeyTitle = "Clave de verificación invalida"
spanishMessage InvalidKey = "Lo sentimos, pero esa clave de verificación es inválida."
spanishMessage InvalidEmailPass = "La combinación cuenta de correo/contraseña es inválida"
spanishMessage BadSetPass = "Debe acceder a la aplicación para modificar la contraseña"
spanishMessage SetPassTitle = "Modificar contraseña"
spanishMessage SetPass = "Actualizar nueva contraseña"
spanishMessage NewPass = "Nueva contraseña"
spanishMessage ConfirmPass = "Confirmar"
spanishMessage PassMismatch = "Las contraseñas no coinciden, inténtelo de nuevo"
spanishMessage PassUpdated = "Contraseña actualizada"
spanishMessage Facebook = "Entrar mediante Facebook"
spanishMessage LoginViaEmail = "Entrar mediante una cuenta de correo"
spanishMessage InvalidLogin = "Login inválido"
spanishMessage NowLoggedIn = "Usted ha ingresado al sitio"
spanishMessage LoginTitle = "Log In"
spanishMessage PleaseProvideUsername = "Por favor escriba su nombre de usuario"
spanishMessage PleaseProvidePassword = "Por favor escriba su contraseña"
spanishMessage NoIdentifierProvided = "No ha indicado una cuenta de correo/nombre de usuario"
spanishMessage InvalidEmailAddress = "La cuenta de correo es inválida"
spanishMessage PasswordResetTitle = "Actualización de contraseña"
spanishMessage ProvideIdentifier = "Cuenta de correo o nombre de usuario"
spanishMessage SendPasswordResetEmail = "Enviar correo de actualización de contraseña"
spanishMessage PasswordResetPrompt = "Escriba su cuenta de correo o nombre de usuario, y una confirmación de actualización de contraseña será enviada a su cuenta de correo."
spanishMessage InvalidUsernamePass = "Combinación de nombre de usuario/contraseña invalida"
-- TODO
spanishMessage i@(IdentifierNotFound _) = englishMessage i
spanishMessage Logout = "Finalizar la sesión" -- FIXME by Google Translate
spanishMessage LogoutTitle = "Finalizar la sesión" -- FIXME by Google Translate
spanishMessage AuthError = "Error de autenticación" -- FIXME by Google Translate

swedishMessage :: AuthMessage -> Text
swedishMessage NoOpenID = "Fann ej OpenID identifierare"
swedishMessage LoginOpenID = "Logga in via OpenID"
swedishMessage LoginGoogle = "Logga in via Google"
swedishMessage LoginYahoo = "Logga in via Yahoo"
swedishMessage Email = "Epost"
swedishMessage UserName = "Användarnamn"  -- FIXME by Google Translate "user name"
swedishMessage Password = "Lösenord"
swedishMessage CurrentPassword = "Current password"
swedishMessage Register = "Registrera"
swedishMessage RegisterLong = "Registrera ett nytt konto"
swedishMessage EnterEmail = "Skriv in din epost nedan så kommer ett konfirmationsmail skickas till adressen."
swedishMessage ConfirmationEmailSentTitle = "Konfirmationsmail skickat"
swedishMessage (ConfirmationEmailSent email) =
    "Ett konfirmationsmeddelande har skickats till" `mappend`
    email `mappend`
    "."
swedishMessage AddressVerified = "Adress verifierad, vänligen välj nytt lösenord"
swedishMessage EmailVerifiedChangePass = "Adress verifierad, vänligen välj nytt lösenord"
swedishMessage EmailVerified = "Adress verifierad"
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
swedishMessage NoIdentifierProvided = "Emailadress eller användarnamn saknas"
swedishMessage InvalidEmailAddress = "Ogiltig emailadress angiven"
swedishMessage PasswordResetTitle = "Återställning av lösenord"
swedishMessage ProvideIdentifier = "Epost eller användarnamn"
swedishMessage SendPasswordResetEmail = "Skicka email för återställning av lösenord"
swedishMessage PasswordResetPrompt = "Skriv in din emailadress eller användarnamn nedan och " `mappend`
                                     "ett email för återställning av lösenord kommmer att skickas till dig."
swedishMessage InvalidUsernamePass = "Ogiltig kombination av användarnamn och lösenord"
-- TODO
swedishMessage i@(IdentifierNotFound _) = englishMessage i
swedishMessage Logout = "Loggar ut" -- FIXME by Google Translate
swedishMessage LogoutTitle = "Loggar ut" -- FIXME by Google Translate
swedishMessage AuthError = "Autentisering Fel" -- FIXME by Google Translate

germanMessage :: AuthMessage -> Text
germanMessage NoOpenID = "Kein OpenID-Identifier gefunden"
germanMessage LoginOpenID = "Login via OpenID"
germanMessage LoginGoogle = "Login via Google"
germanMessage LoginYahoo = "Login via Yahoo"
germanMessage Email = "E-Mail"
germanMessage UserName = "Benutzername"
germanMessage Password = "Passwort"
germanMessage CurrentPassword = "Aktuelles Passwort"
germanMessage Register = "Registrieren"
germanMessage RegisterLong = "Neuen Account registrieren"
germanMessage EnterEmail = "Bitte die E-Mail Adresse angeben, eine Bestätigungsmail wird verschickt."
germanMessage ConfirmationEmailSentTitle = "Bestätigung verschickt."
germanMessage (ConfirmationEmailSent email) =
    "Eine Bestätigung wurde an " `mappend`
    email `mappend`
    " versandt."
germanMessage AddressVerified = "Adresse bestätigt, bitte neues Passwort angeben"
germanMessage EmailVerifiedChangePass = "Adresse bestätigt, bitte neues Passwort angeben"
germanMessage EmailVerified = "Adresse bestätigt"
germanMessage InvalidKeyTitle = "Ungültiger Bestätigungsschlüssel"
germanMessage InvalidKey = "Das war leider ein ungültiger Bestätigungsschlüssel"
germanMessage InvalidEmailPass = "Ungültiger Nutzername oder Passwort"
germanMessage BadSetPass = "Um das Passwort zu ändern muss man eingeloggt sein"
germanMessage SetPassTitle = "Passwort angeben"
germanMessage SetPass = "Neues Passwort angeben"
germanMessage NewPass = "Neues Passwort"
germanMessage ConfirmPass = "Bestätigen"
germanMessage PassMismatch = "Die Passwörter stimmen nicht überein"
germanMessage PassUpdated = "Passwort überschrieben"
germanMessage Facebook = "Login über Facebook"
germanMessage LoginViaEmail = "Login via E-Mail"
germanMessage InvalidLogin = "Ungültiger Login"
germanMessage NowLoggedIn = "Login erfolgreich"
germanMessage LoginTitle = "Anmelden"
germanMessage PleaseProvideUsername = "Bitte Nutzername angeben"
germanMessage PleaseProvidePassword = "Bitte Passwort angeben"
germanMessage NoIdentifierProvided = "Keine E-Mail-Adresse oder kein Nutzername angegeben"
germanMessage InvalidEmailAddress = "Unzulässiger E-Mail-Anbieter"
germanMessage PasswordResetTitle = "Passwort zurücksetzen"
germanMessage ProvideIdentifier = "E-Mail-Adresse oder Nutzername"
germanMessage SendPasswordResetEmail = "E-Mail zusenden um Passwort zurückzusetzen"
germanMessage PasswordResetPrompt = "Nach Einhabe der E-Mail-Adresse oder des Nutzernamen wird eine E-Mail zugesendet mit welcher das Passwort zurückgesetzt werden kann."
germanMessage InvalidUsernamePass = "Ungültige Kombination aus Nutzername und Passwort"
germanMessage i@(IdentifierNotFound _) = englishMessage i -- TODO
germanMessage Logout = "Abmelden"
germanMessage LogoutTitle = "Abmelden"
germanMessage AuthError = "Fehler beim Anmelden"

frenchMessage :: AuthMessage -> Text
frenchMessage NoOpenID = "Aucun fournisseur OpenID n'a été trouvé"
frenchMessage LoginOpenID = "Se connecter avec OpenID"
frenchMessage LoginGoogle = "Se connecter avec Google"
frenchMessage LoginYahoo = "Se connecter avec Yahoo"
frenchMessage Email = "Adresse électronique"
frenchMessage UserName = "Nom d'utilisateur" -- FIXME by Google Translate "user name"
frenchMessage Password = "Mot de passe"
frenchMessage CurrentPassword = "Mot de passe actuel"
frenchMessage Register = "S'inscrire"
frenchMessage RegisterLong = "Créer un compte"
frenchMessage EnterEmail = "Entrez ci-dessous votre adresse électronique, et un message de confirmation vous sera envoyé"
frenchMessage ConfirmationEmailSentTitle = "Message de confirmation"
frenchMessage (ConfirmationEmailSent email) =
    "Un message de confirmation a été envoyé à " `mappend`
    email `mappend`
    "."
frenchMessage AddressVerified = "Votre adresse électronique a été validée, merci de choisir un nouveau mot de passe."
frenchMessage EmailVerifiedChangePass = "Votre adresse électronique a été validée, merci de choisir un nouveau mot de passe."
frenchMessage EmailVerified = "Votre adresse électronique a été validée"
frenchMessage InvalidKeyTitle = "Clef de validation incorrecte"
frenchMessage InvalidKey = "Désolé, mais cette clef de validation est incorrecte"
frenchMessage InvalidEmailPass = "La combinaison de ce mot de passe et de cette adresse électronique n'existe pas."
frenchMessage BadSetPass = "Vous devez être connecté pour choisir un mot de passe"
frenchMessage SetPassTitle = "Changer de mot de passe"
frenchMessage SetPass = "Choisir un nouveau mot de passe"
frenchMessage NewPass = "Nouveau mot de passe"
frenchMessage ConfirmPass = "Confirmation du mot de passe"
frenchMessage PassMismatch = "Le deux mots de passe sont différents, veuillez les corriger"
frenchMessage PassUpdated = "Le mot de passe a bien été changé"
frenchMessage Facebook = "Se connecter avec Facebook"
frenchMessage LoginViaEmail = "Se connecter avec une adresse électronique"
frenchMessage InvalidLogin = "Nom d'utilisateur incorrect"
frenchMessage NowLoggedIn = "Vous êtes maintenant connecté"
frenchMessage LoginTitle = "Se connecter"
frenchMessage PleaseProvideUsername = "Veuillez fournir votre nom d'utilisateur"
frenchMessage PleaseProvidePassword = "Veuillez fournir votre mot de passe"
frenchMessage NoIdentifierProvided = "Adresse électronique/nom d'utilisateur non spécifié"
frenchMessage InvalidEmailAddress = "Adresse électronique spécifiée invalide"
frenchMessage PasswordResetTitle = "Réinitialisation du mot de passe"
frenchMessage ProvideIdentifier = "Adresse électronique ou nom d'utilisateur"
frenchMessage SendPasswordResetEmail = "Envoi d'un courriel pour réinitialiser le mot de passe"
frenchMessage PasswordResetPrompt = "Entrez votre courriel ou votre nom d'utilisateur ci-dessous, et vous recevrez un message électronique pour réinitialiser votre mot de passe."
frenchMessage InvalidUsernamePass = "La combinaison de ce mot de passe et de ce nom d'utilisateur n'existe pas."
frenchMessage (IdentifierNotFound ident) = "Nom d'utilisateur introuvable: " `mappend` ident
frenchMessage Logout = "Déconnexion"
frenchMessage LogoutTitle = "Déconnexion"
frenchMessage AuthError = "Erreur d'authentification" -- FIXME by Google Translate

norwegianBokmålMessage :: AuthMessage -> Text
norwegianBokmålMessage NoOpenID = "Ingen OpenID-identifiserer funnet"
norwegianBokmålMessage LoginOpenID = "Logg inn med OpenID"
norwegianBokmålMessage LoginGoogle = "Logg inn med Google"
norwegianBokmålMessage LoginYahoo = "Logg inn med Yahoo"
norwegianBokmålMessage Email = "E-post"
norwegianBokmålMessage UserName = "Brukernavn" -- FIXME by Google Translate "user name"
norwegianBokmålMessage Password = "Passord"
norwegianBokmålMessage CurrentPassword = "Current password"
norwegianBokmålMessage Register = "Registrer"
norwegianBokmålMessage RegisterLong = "Registrer en ny konto"
norwegianBokmålMessage EnterEmail = "Skriv inn e-postadressen din nedenfor og en e-postkonfirmasjon vil bli sendt."
norwegianBokmålMessage ConfirmationEmailSentTitle = "E-postkonfirmasjon sendt."
norwegianBokmålMessage (ConfirmationEmailSent email) =
    "En e-postkonfirmasjon har blitt sendt til " `mappend`
    email `mappend`
    "."
norwegianBokmålMessage AddressVerified = "Adresse verifisert, vennligst sett et nytt passord."
norwegianBokmålMessage EmailVerifiedChangePass = "Adresse verifisert, vennligst sett et nytt passord."
norwegianBokmålMessage EmailVerified = "Adresse verifisert"
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
norwegianBokmålMessage NoIdentifierProvided = "No email/username provided"
norwegianBokmålMessage InvalidEmailAddress = "Invalid email address provided"
norwegianBokmålMessage PasswordResetTitle = "Password Reset"
norwegianBokmålMessage ProvideIdentifier = "Email or Username"
norwegianBokmålMessage SendPasswordResetEmail = "Send password reset email"
norwegianBokmålMessage PasswordResetPrompt = "Enter your e-mail address or username below, and a password reset e-mail will be sent to you."
norwegianBokmålMessage InvalidUsernamePass = "Invalid username/password combination"
-- TODO
norwegianBokmålMessage i@(IdentifierNotFound _) = englishMessage i
norwegianBokmålMessage Logout = "Logge ut" -- FIXME by Google Translate
norwegianBokmålMessage LogoutTitle = "Logge ut" -- FIXME by Google Translate
norwegianBokmålMessage AuthError = "Godkjenningsfeil" -- FIXME by Google Translate

japaneseMessage :: AuthMessage -> Text
japaneseMessage NoOpenID = "OpenID識別子がありません"
japaneseMessage LoginOpenID = "OpenIDでログイン"
japaneseMessage LoginGoogle = "Googleでログイン"
japaneseMessage LoginYahoo = "Yahooでログイン"
japaneseMessage Email = "Eメール"
japaneseMessage UserName = "ユーザー名" -- FIXME by Google Translate "user name"
japaneseMessage Password = "パスワード"
japaneseMessage CurrentPassword = "現在のパスワード"
japaneseMessage Register = "登録"
japaneseMessage RegisterLong = "新規アカウント登録"
japaneseMessage EnterEmail = "メールアドレスを入力してください。確認メールが送られます"
japaneseMessage ConfirmationEmailSentTitle = "確認メールを送信しました"
japaneseMessage (ConfirmationEmailSent email) =
    "確認メールを " `mappend`
    email `mappend`
    " に送信しました"
japaneseMessage AddressVerified = "アドレスは認証されました。新しいパスワードを設定してください"
japaneseMessage EmailVerifiedChangePass = "アドレスは認証されました。新しいパスワードを設定してください"
japaneseMessage EmailVerified = "アドレスは認証されました"
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
japaneseMessage NoIdentifierProvided = "メールアドレス/ユーザ名が入力されていません"
japaneseMessage InvalidEmailAddress = "メールアドレスが無効です"
japaneseMessage PasswordResetTitle = "パスワードの再設定"
japaneseMessage ProvideIdentifier = "メールアドレスまたはユーザ名"
japaneseMessage SendPasswordResetEmail = "パスワード再設定用メールの送信"
japaneseMessage PasswordResetPrompt = "以下にメールアドレスまたはユーザ名を入力してください。パスワードを再設定するためのメールが送信されます。"
japaneseMessage InvalidUsernamePass = "ユーザ名とパスワードの組み合わせが間違っています"
japaneseMessage (IdentifierNotFound ident) =
  ident `mappend` "は登録されていません"
japaneseMessage Logout = "ログアウト" -- FIXME by Google Translate
japaneseMessage LogoutTitle = "ログアウト" -- FIXME by Google Translate
japaneseMessage AuthError = "認証エラー" -- FIXME by Google Translate

finnishMessage :: AuthMessage -> Text
finnishMessage NoOpenID = "OpenID-tunnistetta ei löydy"
finnishMessage LoginOpenID = "Kirjaudu OpenID-tilillä"
finnishMessage LoginGoogle = "Kirjaudu Google-tilillä"
finnishMessage LoginYahoo = "Kirjaudu Yahoo-tilillä"
finnishMessage Email = "Sähköposti"
finnishMessage UserName = "Käyttäjätunnus" -- FIXME by Google Translate "user name"
finnishMessage Password = "Salasana"
finnishMessage CurrentPassword = "Current password"
finnishMessage Register = "Luo uusi"
finnishMessage RegisterLong = "Luo uusi tili"
finnishMessage EnterEmail = "Kirjoita alle sähköpostiosoitteesi, johon vahvistussähköposti lähetetään."
finnishMessage ConfirmationEmailSentTitle = "Vahvistussähköposti lähetetty."
finnishMessage (ConfirmationEmailSent email) =
    "Vahvistussähköposti on lähetty osoitteeseen " `mappend`
    email `mappend`
    "."

finnishMessage AddressVerified = "Sähköpostiosoite vahvistettu. Anna uusi salasana"
finnishMessage EmailVerifiedChangePass = "Sähköpostiosoite vahvistettu. Anna uusi salasana"
finnishMessage EmailVerified = "Sähköpostiosoite vahvistettu"
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
finnishMessage NoIdentifierProvided = "Sähköpostiosoite/käyttäjänimi puuttuu"
finnishMessage InvalidEmailAddress = "Annettu sähköpostiosoite ei kelpaa"
finnishMessage PasswordResetTitle = "Uuden salasanan tilaaminen"
finnishMessage ProvideIdentifier = "Sähköpostiosoite tai käyttäjänimi"
finnishMessage SendPasswordResetEmail = "Lähetä uusi salasana sähköpostitse"
finnishMessage PasswordResetPrompt = "Anna sähköpostiosoitteesi tai käyttäjätunnuksesi alla, niin lähetämme uuden salasanan sähköpostitse."
finnishMessage InvalidUsernamePass = "Virheellinen käyttäjänimi tai salasana."
-- TODO
finnishMessage i@(IdentifierNotFound _) = englishMessage i
finnishMessage Logout = "Kirjaudu ulos" -- FIXME by Google Translate
finnishMessage LogoutTitle = "Kirjaudu ulos" -- FIXME by Google Translate
finnishMessage AuthError = "Authentication Error" -- FIXME by Google Translate

chineseMessage :: AuthMessage -> Text
chineseMessage NoOpenID = "无效的OpenID"
chineseMessage LoginOpenID = "用OpenID登录"
chineseMessage LoginGoogle = "用Google帐户登录"
chineseMessage LoginYahoo = "用Yahoo帐户登录"
chineseMessage Email = "邮箱"
chineseMessage UserName = "用户名"
chineseMessage Password = "密码"
chineseMessage CurrentPassword = "当前密码"
chineseMessage Register = "注册"
chineseMessage RegisterLong = "注册新帐户"
chineseMessage EnterEmail = "输入你的邮箱地址，你将收到一封确认邮件。"
chineseMessage ConfirmationEmailSentTitle = "确认邮件已发送"
chineseMessage (ConfirmationEmailSent email) =
    "确认邮件已发送至 " `mappend`
    email `mappend`
    "."
chineseMessage AddressVerified = "地址验证成功，请设置新密码"
chineseMessage EmailVerifiedChangePass = "地址验证成功，请设置新密码"
chineseMessage EmailVerified = "地址验证成功"
chineseMessage InvalidKeyTitle = "无效的验证码"
chineseMessage InvalidKey = "对不起，验证码无效。"
chineseMessage InvalidEmailPass = "无效的邮箱/密码组合"
chineseMessage BadSetPass = "你需要登录才能设置密码"
chineseMessage SetPassTitle = "设置密码"
chineseMessage SetPass = "设置新密码"
chineseMessage NewPass = "新密码"
chineseMessage ConfirmPass = "确认"
chineseMessage PassMismatch = "密码不匹配，请重新输入"
chineseMessage PassUpdated = "密码更新成功"
chineseMessage Facebook = "用Facebook帐户登录"
chineseMessage LoginViaEmail = "用邮箱登录"
chineseMessage InvalidLogin = "登录失败"
chineseMessage NowLoggedIn = "登录成功"
chineseMessage LoginTitle = "登录"
chineseMessage PleaseProvideUsername = "请输入用户名"
chineseMessage PleaseProvidePassword = "请输入密码"
chineseMessage NoIdentifierProvided = "缺少邮箱/用户名"
chineseMessage InvalidEmailAddress = "无效的邮箱地址"
chineseMessage PasswordResetTitle = "重置密码"
chineseMessage ProvideIdentifier = "邮箱或用户名"
chineseMessage SendPasswordResetEmail = "发送密码重置邮件"
chineseMessage PasswordResetPrompt = "输入你的邮箱地址或用户名，你将收到一封密码重置邮件。"
chineseMessage InvalidUsernamePass = "无效的用户名/密码组合"
chineseMessage (IdentifierNotFound ident) = "邮箱/用户名不存在: " `mappend` ident
chineseMessage Logout = "注销"
chineseMessage LogoutTitle = "注销"
chineseMessage AuthError = "验证错误"

czechMessage :: AuthMessage -> Text
czechMessage NoOpenID = "Nebyl nalezen identifikátor OpenID"
czechMessage LoginOpenID = "Přihlásit přes OpenID"
czechMessage LoginGoogle = "Přihlásit přes Google"
czechMessage LoginYahoo = "Přihlásit přes Yahoo"
czechMessage Email = "E-mail"
czechMessage UserName = "Uživatelské jméno"
czechMessage Password = "Heslo"
czechMessage CurrentPassword = "Current password"
czechMessage Register = "Registrovat"
czechMessage RegisterLong = "Zaregistrovat nový účet"
czechMessage EnterEmail = "Níže zadejte svou e-mailovou adresu a bude vám poslán potvrzovací e-mail."
czechMessage ConfirmationEmailSentTitle = "Potvrzovací e-mail odeslán"
czechMessage (ConfirmationEmailSent email) =
    "Potvrzovací e-mail byl odeslán na " `mappend` email `mappend` "."
czechMessage AddressVerified = "Adresa byla ověřena, prosím nastavte si nové heslo"
czechMessage EmailVerifiedChangePass = "Adresa byla ověřena, prosím nastavte si nové heslo"
czechMessage EmailVerified = "Adresa byla ověřena"
czechMessage InvalidKeyTitle = "Neplatný ověřovací klíč"
czechMessage InvalidKey = "Bohužel, ověřovací klíč je neplatný."
czechMessage InvalidEmailPass = "Neplatná kombinace e-mail/heslo"
czechMessage BadSetPass = "Pro nastavení hesla je vyžadováno přihlášení"
czechMessage SetPassTitle = "Nastavit heslo"
czechMessage SetPass = "Nastavit nové heslo"
czechMessage NewPass = "Nové heslo"
czechMessage ConfirmPass = "Potvrdit"
czechMessage PassMismatch = "Hesla si neodpovídají, zkuste to znovu"
czechMessage PassUpdated = "Heslo aktualizováno"
czechMessage Facebook = "Přihlásit přes Facebook"
czechMessage LoginViaEmail = "Přihlásit přes e-mail"
czechMessage InvalidLogin = "Neplatné přihlášení"
czechMessage NowLoggedIn = "Přihlášení proběhlo úspěšně"
czechMessage LoginTitle = "Přihlásit"
czechMessage PleaseProvideUsername = "Prosím, zadejte svoje uživatelské jméno"
czechMessage PleaseProvidePassword = "Prosím, zadejte svoje heslo"
czechMessage NoIdentifierProvided = "Nebyl poskytnut žádný e-mail nebo uživatelské jméno"
czechMessage InvalidEmailAddress = "Zadaná e-mailová adresa je neplatná"
czechMessage PasswordResetTitle = "Obnovení hesla"
czechMessage ProvideIdentifier = "E-mail nebo uživatelské jméno"
czechMessage SendPasswordResetEmail = "Poslat e-mail pro obnovení hesla"
czechMessage PasswordResetPrompt = "Zadejte svou e-mailovou adresu nebo uživatelské jméno a bude vám poslán email pro obnovení hesla."
czechMessage InvalidUsernamePass = "Neplatná kombinace uživatelského jména a hesla"
-- TODO
czechMessage i@(IdentifierNotFound _) = englishMessage i
czechMessage Logout = "Odhlásit" -- FIXME by Google Translate
czechMessage LogoutTitle = "Odhlásit" -- FIXME by Google Translate
czechMessage AuthError = "Chyba ověřování" -- FIXME by Google Translate

-- Так как e-mail – это фактическое сокращение словосочетания electronic mail,
-- для русского перевода так же использовано сокращение: эл.почта
russianMessage :: AuthMessage -> Text
russianMessage NoOpenID = "Идентификатор OpenID не найден"
russianMessage LoginOpenID = "Вход с помощью OpenID"
russianMessage LoginGoogle = "Вход с помощью Google"
russianMessage LoginYahoo = "Вход с помощью Yahoo"
russianMessage Email = "Эл.почта"
russianMessage UserName = "Имя пользователя"
russianMessage Password = "Пароль"
russianMessage CurrentPassword = "Старый пароль"
russianMessage Register = "Регистрация"
russianMessage RegisterLong = "Создать учётную запись"
russianMessage EnterEmail = "Введите свой адрес эл.почты ниже, вам будет отправлено письмо для подтверждения."
russianMessage ConfirmationEmailSentTitle = "Письмо для подтверждения отправлено"
russianMessage (ConfirmationEmailSent email) =
    "Письмо для подтверждения было отправлено на адрес " `mappend`
    email `mappend`
    "."
russianMessage AddressVerified = "Адрес подтверждён. Пожалуйста, установите новый пароль."
russianMessage EmailVerifiedChangePass = "Адрес подтверждён. Пожалуйста, установите новый пароль."
russianMessage EmailVerified = "Адрес подтверждён"
russianMessage InvalidKeyTitle = "Неверный ключ подтверждения"
russianMessage InvalidKey = "Извините, но ключ подтверждения оказался недействительным."
russianMessage InvalidEmailPass = "Неверное сочетание эл.почты и пароля"
russianMessage BadSetPass = "Чтобы изменить пароль, необходимо выполнить вход"
russianMessage SetPassTitle = "Установить пароль"
russianMessage SetPass = "Установить новый пароль"
russianMessage NewPass = "Новый пароль"
russianMessage ConfirmPass = "Подтверждение пароля"
russianMessage PassMismatch = "Пароли не совпадают, повторите снова"
russianMessage PassUpdated = "Пароль обновлён"
russianMessage Facebook = "Войти с помощью Facebook"
russianMessage LoginViaEmail = "Войти по адресу эл.почты"
russianMessage InvalidLogin = "Неверный логин"
russianMessage NowLoggedIn = "Вход выполнен"
russianMessage LoginTitle = "Войти"
russianMessage PleaseProvideUsername = "Пожалуйста, введите ваше имя пользователя"
russianMessage PleaseProvidePassword = "Пожалуйста, введите ваш пароль"
russianMessage NoIdentifierProvided = "Не указан адрес эл.почты/имя пользователя"
russianMessage InvalidEmailAddress = "Указан неверный адрес эл.почты"
russianMessage PasswordResetTitle = "Сброс пароля"
russianMessage ProvideIdentifier = "Имя пользователя или эл.почта"
russianMessage SendPasswordResetEmail = "Отправить письмо для сброса пароля"
russianMessage PasswordResetPrompt = "Введите адрес эл.почты или ваше имя пользователя ниже, вам будет отправлено письмо для сброса пароля."
russianMessage InvalidUsernamePass = "Неверное сочетание имени пользователя и пароля"
russianMessage (IdentifierNotFound ident) = "Логин не найден: " `mappend` ident
russianMessage Logout = "Выйти"
russianMessage LogoutTitle = "Выйти"
russianMessage AuthError = "Ошибка аутентификации"

dutchMessage :: AuthMessage -> Text
dutchMessage NoOpenID = "Geen OpenID identificator gevonden"
dutchMessage LoginOpenID = "Inloggen via OpenID"
dutchMessage LoginGoogle = "Inloggen via Google"
dutchMessage LoginYahoo = "Inloggen via Yahoo"
dutchMessage Email = "E-mail"
dutchMessage UserName = "Gebruikersnaam"
dutchMessage Password = "Wachtwoord"
dutchMessage CurrentPassword = "Huidig wachtwoord"
dutchMessage Register = "Registreren"
dutchMessage RegisterLong = "Registreer een nieuw account"
dutchMessage EnterEmail = "Voer uw e-mailadres hieronder in, er zal een bevestigings-e-mail naar u worden verzonden."
dutchMessage ConfirmationEmailSentTitle = "Bevestigings-e-mail verzonden"
dutchMessage (ConfirmationEmailSent email) =
    "Een bevestigings-e-mail is verzonden naar " `mappend`
    email `mappend`
    "."
dutchMessage AddressVerified = "Adres geverifieerd, stel alstublieft een nieuwe wachtwoord in"
dutchMessage EmailVerifiedChangePass = "Adres geverifieerd, stel alstublieft een nieuwe wachtwoord in"
dutchMessage EmailVerified = "Adres geverifieerd"
dutchMessage InvalidKeyTitle = "Ongeldig verificatietoken"
dutchMessage InvalidKey = "Dat was helaas een ongeldig verificatietoken."
dutchMessage InvalidEmailPass = "Ongeldige e-mailadres/wachtwoord combinatie"
dutchMessage BadSetPass = "U moet ingelogd zijn om een nieuwe wachtwoord in te stellen"
dutchMessage SetPassTitle = "Wachtwoord instellen"
dutchMessage SetPass = "Een nieuwe wachtwoord instellen"
dutchMessage NewPass = "Nieuw wachtwoord"
dutchMessage ConfirmPass = "Bevestig"
dutchMessage PassMismatch = "Wachtwoorden kwamen niet overeen, probeer het alstublieft nog eens"
dutchMessage PassUpdated = "Wachtwoord geüpdatet"
dutchMessage Facebook = "Inloggen met Facebook"
dutchMessage LoginViaEmail = "Inloggen via e-mail"
dutchMessage InvalidLogin = "Ongeldige inloggegevens"
dutchMessage NowLoggedIn = "U bent nu ingelogd"
dutchMessage LoginTitle = "Inloggen"
dutchMessage PleaseProvideUsername = "Voer alstublieft uw gebruikersnaam in"
dutchMessage PleaseProvidePassword = "Voer alstublieft uw wachtwoord in"
dutchMessage NoIdentifierProvided = "Geen e-mailadres/gebruikersnaam opgegeven"
dutchMessage InvalidEmailAddress = "Ongeldig e-mailadres opgegeven"
dutchMessage PasswordResetTitle = "Wachtwoord wijzigen"
dutchMessage ProvideIdentifier = "E-mailadres of gebruikersnaam"
dutchMessage SendPasswordResetEmail = "Stuur een wachtwoord reset e-mail"
dutchMessage PasswordResetPrompt = "Voer uw e-mailadres of gebruikersnaam hieronder in, er zal een e-mail naar u worden verzonden waarmee u uw wachtwoord kunt wijzigen."
dutchMessage InvalidUsernamePass = "Ongeldige gebruikersnaam/wachtwoord combinatie"
dutchMessage (IdentifierNotFound ident) = "Inloggegevens niet gevonden: " `mappend` ident
dutchMessage Logout = "Uitloggen"
dutchMessage LogoutTitle = "Uitloggen"
dutchMessage AuthError = "Verificatiefout"

croatianMessage :: AuthMessage -> Text
croatianMessage NoOpenID = "Nije pronađen OpenID identifikator"
croatianMessage LoginOpenID = "Prijava uz OpenID"
croatianMessage LoginGoogle = "Prijava uz Google"
croatianMessage LoginYahoo = "Prijava uz Yahoo"
croatianMessage Facebook = "Prijava uz Facebook"
croatianMessage LoginViaEmail = "Prijava putem e-pošte"
croatianMessage Email = "E-pošta"
croatianMessage UserName = "Korisničko ime"
croatianMessage Password = "Lozinka"
croatianMessage CurrentPassword = "Current Password"
croatianMessage Register = "Registracija"
croatianMessage RegisterLong = "Registracija novog računa"
croatianMessage EnterEmail = "Dolje unesite adresu e-pošte, pa ćemo vam poslati e-poruku za potvrdu."
croatianMessage PasswordResetPrompt = "Dolje unesite adresu e-pošte ili korisničko ime, pa ćemo vam poslati e-poruku za potvrdu."
croatianMessage ConfirmationEmailSentTitle = "E-poruka za potvrdu"
croatianMessage (ConfirmationEmailSent email) = "E-poruka za potvrdu poslana je na adresu " <> email <> "."
croatianMessage AddressVerified = "Adresa ovjerena, postavite novu lozinku"
croatianMessage EmailVerifiedChangePass = "Adresa ovjerena, postavite novu lozinku"
croatianMessage EmailVerified = "Adresa ovjerena"
croatianMessage InvalidKeyTitle = "Ključ za ovjeru nije valjan"
croatianMessage InvalidKey = "Nažalost, taj ključ za ovjeru nije valjan."
croatianMessage InvalidEmailPass = "Kombinacija e-pošte i lozinke nije valjana"
croatianMessage InvalidUsernamePass = "Kombinacija korisničkog imena i lozinke nije valjana"
croatianMessage BadSetPass = "Za postavljanje lozinke morate biti prijavljeni"
croatianMessage SetPassTitle = "Postavi lozinku"
croatianMessage SetPass = "Postavite novu lozinku"
croatianMessage NewPass = "Nova lozinka"
croatianMessage ConfirmPass = "Potvrda lozinke"
croatianMessage PassMismatch = "Lozinke se ne podudaraju, pokušajte ponovo"
croatianMessage PassUpdated = "Lozinka ažurirana"
croatianMessage InvalidLogin = "Prijava nije valjana"
croatianMessage NowLoggedIn = "Sada ste prijavljeni u"
croatianMessage LoginTitle = "Prijava"
croatianMessage PleaseProvideUsername = "Unesite korisničko ime"
croatianMessage PleaseProvidePassword = "Unesite lozinku"
croatianMessage NoIdentifierProvided = "Nisu dani e-pošta/korisničko ime"
croatianMessage InvalidEmailAddress = "Dana adresa e-pošte nije valjana"
croatianMessage PasswordResetTitle = "Poništavanje lozinke"
croatianMessage ProvideIdentifier = "E-pošta ili korisničko ime"
croatianMessage SendPasswordResetEmail = "Pošalji e-poruku za poništavanje lozinke"
croatianMessage (IdentifierNotFound ident) = "Korisničko ime/e-pošta nisu pronađeni: " <> ident
croatianMessage Logout = "Odjava"
croatianMessage LogoutTitle = "Odjava"
croatianMessage AuthError = "Pogreška provjere autentičnosti"

danishMessage :: AuthMessage -> Text
danishMessage NoOpenID = "Mangler OpenID identifier"
danishMessage LoginOpenID = "Login med OpenID"
danishMessage LoginGoogle = "Login med Google"
danishMessage LoginYahoo = "Login med Yahoo"
danishMessage Email = "E-mail"
danishMessage UserName = "Brugernavn"
danishMessage Password = "Kodeord"
danishMessage CurrentPassword = "Nuværende kodeord"
danishMessage Register = "Opret"
danishMessage RegisterLong = "Opret en ny konto"
danishMessage EnterEmail = "Indtast din e-mailadresse nedenfor og en bekræftelsesmail vil blive sendt til dig."
danishMessage ConfirmationEmailSentTitle = "Bekræftelsesmail sendt"
danishMessage (ConfirmationEmailSent email) =
    "En bekræftelsesmail er sendt til " `mappend`
    email `mappend`
    "."
danishMessage AddressVerified = "Adresse bekræftet, sæt venligst et nyt kodeord"
danishMessage EmailVerifiedChangePass = "Adresse bekræftet, sæt venligst et nyt kodeord"
danishMessage EmailVerified = "Adresse bekræftet"
danishMessage InvalidKeyTitle = "Ugyldig verifikationsnøgle"
danishMessage InvalidKey = "Beklager, det var en ugyldigt verifikationsnøgle."
danishMessage InvalidEmailPass = "Ugyldigt e-mail/kodeord"
danishMessage BadSetPass = "Du skal være logget ind for at sætte et kodeord"
danishMessage SetPassTitle = "Sæt kodeord"
danishMessage SetPass = "Sæt et nyt kodeord"
danishMessage NewPass = "Nyt kodeord"
danishMessage ConfirmPass = "Bekræft"
danishMessage PassMismatch = "Kodeordne var forskellige, prøv venligst igen"
danishMessage PassUpdated = "Kodeord opdateret"
danishMessage Facebook = "Login med Facebook"
danishMessage LoginViaEmail = "Login med e-mail"
danishMessage InvalidLogin = "Ugyldigt login"
danishMessage NowLoggedIn = "Du er nu logget ind"
danishMessage LoginTitle = "Log ind"
danishMessage PleaseProvideUsername = "Indtast venligst dit brugernavn"
danishMessage PleaseProvidePassword = "Indtasy venligst dit kodeord"
danishMessage NoIdentifierProvided = "Mangler e-mail/username"
danishMessage InvalidEmailAddress = "Ugyldig e-mailadresse indtastet"
danishMessage PasswordResetTitle = "Nulstilning af kodeord"
danishMessage ProvideIdentifier = "E-mail eller brugernavn"
danishMessage SendPasswordResetEmail = "Send kodeordsnulstillingsmail"
danishMessage PasswordResetPrompt = "Indtast din e-mailadresse eller dit brugernavn nedenfor, så bliver en kodeordsnulstilningsmail sendt til dig."
danishMessage InvalidUsernamePass = "Ugyldigt brugernavn/kodeord"
danishMessage (IdentifierNotFound ident) = "Brugernavn findes ikke: " `mappend` ident
danishMessage Logout = "Log ud"
danishMessage LogoutTitle = "Log ud"
danishMessage AuthError = "Fejl ved bekræftelse af identitet"

koreanMessage :: AuthMessage -> Text
koreanMessage NoOpenID = "OpenID ID가 없습니다"
koreanMessage LoginOpenID = "OpenID로 로그인"
koreanMessage LoginGoogle = "Google로 로그인"
koreanMessage LoginYahoo = "Yahoo로 로그인"
koreanMessage Email = "이메일"
koreanMessage UserName = "사용자 이름"
koreanMessage Password = "비밀번호"
koreanMessage CurrentPassword = "현재 비밀번호"
koreanMessage Register = "등록"
koreanMessage RegisterLong = "새 계정 등록"
koreanMessage EnterEmail = "이메일 주소를 아래에 입력하시면 확인 이메일이 발송됩니다."
koreanMessage ConfirmationEmailSentTitle = "확인 이메일을 보냈습니다"
koreanMessage (ConfirmationEmailSent email) =
    "확인 이메일을 " `mappend`
    email `mappend`
    "에 보냈습니다."
koreanMessage AddressVerified = "주소가 인증되었습니다. 새 비밀번호를 설정하세요."
koreanMessage EmailVerifiedChangePass = "주소가 인증되었습니다. 새 비밀번호를 설정하세요."
koreanMessage EmailVerified = "주소가 인증되었습니다"
koreanMessage InvalidKeyTitle = "인증키가 잘못되었습니다"
koreanMessage InvalidKey = "죄송합니다. 잘못된 인증키입니다."
koreanMessage InvalidEmailPass = "이메일 주소나 비밀번호가 잘못되었습니다"
koreanMessage BadSetPass = "비밀번호를 설정하기 위해서는 로그인해야 합니다"
koreanMessage SetPassTitle = "비밀번호 설정"
koreanMessage SetPass = "새 비밀번호 설정"
koreanMessage NewPass = "새 비밀번호"
koreanMessage ConfirmPass = "확인"
koreanMessage PassMismatch = "비밀번호가 맞지 않습니다. 다시 시도해주세요."
koreanMessage PassUpdated = "비밀번호가 업데이트 되었습니다"
koreanMessage Facebook = "Facebook으로 로그인"
koreanMessage LoginViaEmail = "이메일로"
koreanMessage InvalidLogin = "잘못된 로그인입니다"
koreanMessage NowLoggedIn = "로그인했습니다"
koreanMessage LoginTitle = "로그인"
koreanMessage PleaseProvideUsername = "사용자 이름을 입력하세요"
koreanMessage PleaseProvidePassword = "비밀번호를 입력하세요"
koreanMessage NoIdentifierProvided = "이메일 주소나 사용자 이름이 입력되어 있지 않습니다"
koreanMessage InvalidEmailAddress = "이메일 주소가 잘못되었습니다"
koreanMessage PasswordResetTitle = "비밀번호 변경"
koreanMessage ProvideIdentifier = "이메일 주소나 사용자 이름"
koreanMessage SendPasswordResetEmail = "비밀번호 재설정 이메일 보내기"
koreanMessage PasswordResetPrompt = "이메일 주소나 사용자 이름을 아래에 입력하시면 비밀번호 재설정 이메일이 발송됩니다."
koreanMessage InvalidUsernamePass = "사용자 이름이나 비밀번호가 잘못되었습니다"
koreanMessage (IdentifierNotFound ident) = ident `mappend` "는 등록되어 있지 않습니다"
koreanMessage Logout = "로그아웃"
koreanMessage LogoutTitle = "로그아웃"
koreanMessage AuthError = "인증오류"
