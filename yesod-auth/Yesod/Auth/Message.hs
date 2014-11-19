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
    , spanishMessage
    , czechMessage
    , russianMessage
    , dutchMessage
    ) where

import Data.Monoid (mappend)
import Data.Text (Text)

data AuthMessage =
      NoOpenID
    | LoginOpenID
    | LoginGoogle
    | LoginYahoo
    | Email
    | IdentifierNotFound Text
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
    | NoIdentifierProvided
    | InvalidEmailAddress
    | PasswordResetTitle
    | ProvideIdentifier
    | SendPasswordResetEmail
    | PasswordResetPrompt
    | InvalidUsernamePass

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
englishMessage NoIdentifierProvided = "No email/username provided"
englishMessage InvalidEmailAddress = "Invalid email address provided"
englishMessage PasswordResetTitle = "Password Reset"
englishMessage ProvideIdentifier = "Email or Username"
englishMessage SendPasswordResetEmail = "Send password reset email"
englishMessage PasswordResetPrompt = "Enter your e-mail address or username below, and a password reset e-mail will be sent to you."
englishMessage InvalidUsernamePass = "Invalid username/password combination"
englishMessage (IdentifierNotFound ident) = "Login not found: " `mappend` ident

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
portugueseMessage NoIdentifierProvided = "Nenhum e-mail ou nome de usuário informado"
portugueseMessage InvalidEmailAddress = "Endereço de e-mail inválido informado"
portugueseMessage PasswordResetTitle = "Resetar senha"
portugueseMessage ProvideIdentifier = "E-mail ou nome de usuário"
portugueseMessage SendPasswordResetEmail = "Enviar e-mail para resetar senha"
portugueseMessage PasswordResetPrompt = "Insira seu endereço de e-mail ou nome de usuário abaixo.  Um e-mail para resetar sua senha será enviado para você."
portugueseMessage InvalidUsernamePass = "Nome de usuário ou senha inválidos"
-- TODO
portugueseMessage i@(IdentifierNotFound _) = englishMessage i

spanishMessage :: AuthMessage -> Text
spanishMessage NoOpenID = "No se encuentra el identificador OpenID"
spanishMessage LoginOpenID = "Entrar utilizando OpenID"
spanishMessage LoginGoogle = "Entrar utilizando Google"
spanishMessage LoginYahoo = "Entrar utilizando Yahoo"
spanishMessage Email = "Correo electrónico"
spanishMessage Password = "Contraseña"
spanishMessage Register = "Registrarse"
spanishMessage RegisterLong = "Registrar una nueva cuenta"
spanishMessage EnterEmail = "Coloque su dirección de correo electrónico, y un correo de confirmación le será enviado a su cuenta."
spanishMessage ConfirmationEmailSentTitle = "La confirmación de correo ha sido enviada"
spanishMessage (ConfirmationEmailSent email) =
    "Una confirmación de correo electrónico ha sido enviada a " `mappend`
    email `mappend`
    "."
spanishMessage AddressVerified = "Dirección verificada, por favor introduzca una contraseña"
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
spanishMessage LoginTitle = "Login"
spanishMessage PleaseProvideUsername = "Por favor escriba su nombre de usuario"
spanishMessage PleaseProvidePassword = "Por favor escriba su contraseña"
spanishMessage NoIdentifierProvided = "No ha indicado una cuenta de correo/nombre de usuario"
spanishMessage InvalidEmailAddress = "La cuenta de correo es inválida"
spanishMessage PasswordResetTitle = "Contraseña actualizada"
spanishMessage ProvideIdentifier = "Cuenta de correo o nombre de usuario"
spanishMessage SendPasswordResetEmail = "Correo de actualización de contraseña enviado"
spanishMessage PasswordResetPrompt = "Escriba su cuenta de correo o nombre de usuario, y una confirmación de actualización de contraseña será enviada a su cuenta de correo."
spanishMessage InvalidUsernamePass = "Combinación de nombre de usuario/contraseña invalida"
-- TODO
spanishMessage i@(IdentifierNotFound _) = englishMessage i

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
germanMessage NoIdentifierProvided = "Keine Email-Adresse oder kein Nutzername angegeben"
germanMessage InvalidEmailAddress = "Unzulässiger Email-Anbieter"
germanMessage PasswordResetTitle = "Passwort zurücksetzen"
germanMessage ProvideIdentifier = "Email-Adresse oder Nutzername"
germanMessage SendPasswordResetEmail = "Email zusenden um Passwort zurückzusetzen"
germanMessage PasswordResetPrompt = "Nach Einhabe der Email-Adresse oder des Nutzernamen wird eine Email zugesendet mit welcher das Passwort zurückgesetzt werden kann."
germanMessage InvalidUsernamePass = "Ungültige Kombination aus Nutzername und Passwort"
-- TODO
germanMessage i@(IdentifierNotFound _) = englishMessage i

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
frenchMessage NoIdentifierProvided = "Adresse électronique/nom d'utilisateur non spécifié"
frenchMessage InvalidEmailAddress = "Adresse électronique spécifiée invalide"
frenchMessage PasswordResetTitle = "Réinitialisation de mot de passe"
frenchMessage ProvideIdentifier = "Adresse électronique ou nom d'utilisateur"
frenchMessage SendPasswordResetEmail = "Envoie d'un message électronique pour Réinitialisation le mot de passe"
frenchMessage PasswordResetPrompt = "Entrez votre adresse électronique ou votre nom d'utilisateur ci-dessous, et un message électronique de réinitialisation de mot de passe vous sera envoyé."
frenchMessage InvalidUsernamePass = "Le couble nom d'utilisateur/mot de passe invalide"
frenchMessage (IdentifierNotFound ident) = "Nom d'utilisateur introuvable: " `mappend` ident

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
norwegianBokmålMessage NoIdentifierProvided = "No email/username provided"
norwegianBokmålMessage InvalidEmailAddress = "Invalid email address provided"
norwegianBokmålMessage PasswordResetTitle = "Password Reset"
norwegianBokmålMessage ProvideIdentifier = "Email or Username"
norwegianBokmålMessage SendPasswordResetEmail = "Send password reset email"
norwegianBokmålMessage PasswordResetPrompt = "Enter your e-mail address or username below, and a password reset e-mail will be sent to you."
norwegianBokmålMessage InvalidUsernamePass = "Invalid username/password combination"
-- TODO
norwegianBokmålMessage i@(IdentifierNotFound _) = englishMessage i

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
japaneseMessage NoIdentifierProvided = "メールアドレス/ユーザ名が入力されていません"
japaneseMessage InvalidEmailAddress = "メールアドレスが無効です"
japaneseMessage PasswordResetTitle = "パスワードの再設定"
japaneseMessage ProvideIdentifier = "メールアドレスまたはユーザ名"
japaneseMessage SendPasswordResetEmail = "パスワード再設定用メールの送信"
japaneseMessage PasswordResetPrompt = "以下にメールアドレスまたはユーザ名を入力してください。パスワードを再設定するためのメールが送信されます。"
japaneseMessage InvalidUsernamePass = "ユーザ名とパスワードの組み合わせが間違っています"
japaneseMessage (IdentifierNotFound ident) =
  ident `mappend` "は登録されていません"

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
finnishMessage NoIdentifierProvided = "Sähköpostiosoite/käyttäjänimi puuttuu"
finnishMessage InvalidEmailAddress = "Annettu sähköpostiosoite ei kelpaa"
finnishMessage PasswordResetTitle = "Uuden salasanan tilaaminen"
finnishMessage ProvideIdentifier = "Sähköpostiosoite tai käyttäjänimi"
finnishMessage SendPasswordResetEmail = "Lähetä uusi salasana sähköpostitse"
finnishMessage PasswordResetPrompt = "Anna sähköpostiosoitteesi tai käyttäjätunnuksesi alla, niin lähetämme uuden salasanan sähköpostitse."
finnishMessage InvalidUsernamePass = "Virheellinen käyttäjänimi tai salasana."
-- TODO
finnishMessage i@(IdentifierNotFound _) = englishMessage i

chineseMessage :: AuthMessage -> Text
chineseMessage NoOpenID = "无效的OpenID"
chineseMessage LoginOpenID = "用OpenID登录"
chineseMessage LoginGoogle = "用Google帐户登录"
chineseMessage LoginYahoo = "用Yahoo帐户登录"
chineseMessage Email = "邮箱"
chineseMessage Password = "密码"
chineseMessage Register = "注册"
chineseMessage RegisterLong = "注册新帐户"
chineseMessage EnterEmail = "输入你的邮箱地址，你将收到一封确认邮件。"
chineseMessage ConfirmationEmailSentTitle = "确认邮件已发送"
chineseMessage (ConfirmationEmailSent email) =
    "确认邮件已发送至 " `mappend`
    email `mappend`
    "."
chineseMessage AddressVerified = "地址验证成功，请设置新密码"
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
-- TODO
chineseMessage i@(IdentifierNotFound _) = englishMessage i

czechMessage :: AuthMessage -> Text
czechMessage NoOpenID = "Nebyl nalezen identifikátor OpenID"
czechMessage LoginOpenID = "Přihlásit přes OpenID"
czechMessage LoginGoogle = "Přihlásit přes Google"
czechMessage LoginYahoo = "Přihlásit přes Yahoo"
czechMessage Email = "E-mail"
czechMessage Password = "Heslo"
czechMessage Register = "Registrovat"
czechMessage RegisterLong = "Zaregistrovat nový účet"
czechMessage EnterEmail = "Níže zadejte svou e-mailovou adresu a bude vám poslán potvrzovací e-mail."
czechMessage ConfirmationEmailSentTitle = "Potvrzovací e-mail odeslán"
czechMessage (ConfirmationEmailSent email) =
    "Potvrzovací e-mail byl odeslán na " `mappend` email `mappend` "."
czechMessage AddressVerified = "Adresa byla ověřena, prosím nastavte si nové heslo"
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

-- Так как e-mail – это фактическое сокращение словосочетания electronic mail,
-- для русского перевода так же использовано сокращение: эл.почта
russianMessage :: AuthMessage -> Text
russianMessage NoOpenID = "Идентификатор OpenID не найден"
russianMessage LoginOpenID = "Вход с помощью OpenID"
russianMessage LoginGoogle = "Вход с помощью Google"
russianMessage LoginYahoo = "Вход с помощью Yahoo"
russianMessage Email = "Эл.почта"
russianMessage Password = "Пароль"
russianMessage Register = "Регистрация"
russianMessage RegisterLong = "Создать учётную запись"
russianMessage EnterEmail = "Введите свой адрес эл.почты ниже, вам будет отправлено письмо для подтверждения."
russianMessage ConfirmationEmailSentTitle = "Письмо для подтверждения отправлено"
russianMessage (ConfirmationEmailSent email) =
    "Письмо для подтверждения было отправлено на адрес " `mappend`
    email `mappend`
    "."
russianMessage AddressVerified = "Адрес подтверждён. Пожалуйста, установите новый пароль."
russianMessage InvalidKeyTitle = "Неверный ключ подтверждения"
russianMessage InvalidKey = "Извините, но ключ подтверждения оказался недействительным."
russianMessage InvalidEmailPass = "Неверное сочетание эл.почты и пароля"
russianMessage BadSetPass = "Чтобы изменить пароль, необходимо выполнить вход"
russianMessage SetPassTitle = "Установить пароль"
russianMessage SetPass = "Установить новый пароль"
russianMessage NewPass = "Новый пароль"
russianMessage ConfirmPass = "Подтверждение"
russianMessage PassMismatch = "Пароли не совпадают, повторите снова"
russianMessage PassUpdated = "Пароль обновлён"
russianMessage Facebook = "Вход с помощью Facebook"
russianMessage LoginViaEmail = "Вход по адресу эл.почты"
russianMessage InvalidLogin = "Неверный логин"
russianMessage NowLoggedIn = "Вход выполнен"
russianMessage LoginTitle = "Вход"
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

dutchMessage :: AuthMessage -> Text
dutchMessage NoOpenID = "Geen OpenID identificator gevonden"
dutchMessage LoginOpenID = "Inloggen via OpenID"
dutchMessage LoginGoogle = "Inloggen via Google"
dutchMessage LoginYahoo = "Inloggen via Yahoo"
dutchMessage Email = "E-mail"
dutchMessage Password = "Wachtwoord"
dutchMessage Register = "Registreren"
dutchMessage RegisterLong = "Registreer een nieuw account"
dutchMessage EnterEmail = "Voer uw e-mailadres hieronder in, er zal een bevestigings-e-mail naar u worden verzonden."
dutchMessage ConfirmationEmailSentTitle = "Bevestigings-e-mail verzonden"
dutchMessage (ConfirmationEmailSent email) =
    "Een bevestigings-e-mail is verzonden naar " `mappend`
    email `mappend`
    "."
dutchMessage AddressVerified = "Adres geverifieerd, stel alstublieft een nieuwe wachtwoord in"
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
