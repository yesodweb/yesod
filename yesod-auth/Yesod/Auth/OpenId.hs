{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Yesod.Auth.OpenId
    ( authOpenId
    , authOpenIdExtended
    , forwardUrl
    , claimedKey
    , credsIdentClaimed
    ) where

import Yesod.Auth
import qualified Web.Authenticate.OpenId as OpenId

import Yesod.Form
import Yesod.Handler
import Yesod.Widget (toWidget, whamlet)
import Yesod.Request
import Text.Cassius (cassius)
#if MIN_VERSION_blaze_html(0, 5, 0)
import Text.Blaze.Html (toHtml)
#else
import Text.Blaze (toHtml)
#endif
import Data.Text (Text, isPrefixOf)
import qualified Yesod.Auth.Message as Msg
import Control.Exception.Lifted (SomeException, try)
import Data.Maybe (fromMaybe)

forwardUrl :: AuthRoute
forwardUrl = PluginR "openid" ["forward"]

authOpenId :: YesodAuth m => AuthPlugin m
authOpenId = authOpenIdExtended []

authOpenIdExtended :: YesodAuth m => [(Text, Text)] -> AuthPlugin m
authOpenIdExtended extensionFields =
    AuthPlugin "openid" dispatch login
  where
    complete = PluginR "openid" ["complete"]
    name = "openid_identifier"
    login tm = do
        ident <- lift newIdent
        toWidget [cassius|##{ident}
    background: #fff url(http://www.myopenid.com/static/openid-icon-small.gif) no-repeat scroll 0pt 50%;
    padding-left: 18px;
|]
        [whamlet|
$newline never
<form method="get" action="@{tm forwardUrl}">
    <input type="hidden" name="openid_identifier" value="https://www.google.com/accounts/o8/id">
    <button .openid-google>_{Msg.LoginGoogle}
<form method="get" action="@{tm forwardUrl}">
    <input type="hidden" name="openid_identifier" value="http://me.yahoo.com">
    <button .openid-yahoo>_{Msg.LoginYahoo}
<form method="get" action="@{tm forwardUrl}">
    <label for="#{ident}">OpenID: #
    <input id="#{ident}" type="text" name="#{name}" value="http://">
    <input type="submit" value="_{Msg.LoginOpenID}">
|]
    dispatch "GET" ["forward"] = do
        roid <- runInputGet $ iopt textField name
        case roid of
            Just oid -> do
                render <- getUrlRender
                toMaster <- getRouteToMaster
                let complete' = render $ toMaster complete
                master <- getYesod
                eres <- lift $ try $ OpenId.getForwardUrl oid complete' Nothing extensionFields (authHttpManager master)
                case eres of
                    Left err -> do
                        setMessage $ toHtml $ show (err :: SomeException)
                        redirect $ toMaster LoginR
                    Right x -> redirect x
            Nothing -> do
                toMaster <- getRouteToMaster
                setMessageI Msg.NoOpenID
                redirect $ toMaster LoginR
    dispatch "GET" ["complete", ""] = dispatch "GET" ["complete"] -- compatibility issues
    dispatch "GET" ["complete"] = do
        rr <- getRequest
        completeHelper $ reqGetParams rr
    dispatch "POST" ["complete", ""] = dispatch "POST" ["complete"] -- compatibility issues
    dispatch "POST" ["complete"] = do
        (posts, _) <- runRequestBody
        completeHelper posts
    dispatch _ _ = notFound

completeHelper :: YesodAuth m => [(Text, Text)] -> GHandler Auth m ()
completeHelper gets' = do
        master <- getYesod
        eres <- lift $ try $ OpenId.authenticateClaimed gets' (authHttpManager master)
        toMaster <- getRouteToMaster
        let onFailure err = do
            setMessage $ toHtml $ show (err :: SomeException)
            redirect $ toMaster LoginR
        let onSuccess oir = do
                let claimed =
                        case OpenId.oirClaimed oir of
                            Nothing -> id
                            Just (OpenId.Identifier i') -> ((claimedKey, i'):)
                    gets'' = claimed $ filter (\(k, _) -> not $ "__" `isPrefixOf` k) gets'
                    i = OpenId.identifier $ OpenId.oirOpLocal oir
                setCreds True $ Creds "openid" i gets''
        either onFailure onSuccess eres

-- | The main identifier provided by the OpenID authentication plugin is the
-- \"OP-local identifier\". There is also sometimes a \"claimed\" identifier
-- available.
--
-- In the 'credsExtra' field of the 'Creds' datatype, you can lookup this key
-- to find the claimed identifier, if available.
--
-- > let finalID = fromMaybe (credsIdent creds)
-- >             $ lookup claimedKey (credsExtra creds)
--
-- Since 1.0.2
claimedKey :: Text
claimedKey = "__CLAIMED"

-- | A helper function which will get the claimed identifier, if available, falling back to the OP local identifier.
--
-- See 'claimedKey'.
--
-- Since 1.0.2
credsIdentClaimed :: Creds m -> Text

-- Prevent other backends from overloading the __CLAIMED value, which could
-- possibly open us to security holes.
credsIdentClaimed c | credsPlugin c /= "openid" = credsIdent c

credsIdentClaimed c = fromMaybe (credsIdent c)
                    $ lookup claimedKey (credsExtra c)
