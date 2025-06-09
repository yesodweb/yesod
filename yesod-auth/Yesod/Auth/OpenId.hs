{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Yesod.Auth.OpenId
    ( authOpenId
    , forwardUrl
    , claimedKey
    , opLocalKey
    , credsIdentClaimed
    , IdentifierType (..)
    ) where

import Yesod.Auth
import qualified Web.Authenticate.OpenId as OpenId

import Yesod.Form
import Yesod.Core
import Data.Text (Text, isPrefixOf)
import qualified Yesod.Auth.Message as Msg
import UnliftIO.Exception (tryAny)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

forwardUrl :: AuthRoute
forwardUrl = PluginR "openid" ["forward"]

data IdentifierType = Claimed | OPLocal

authOpenId :: YesodAuth master
           => IdentifierType
           -> [(Text, Text)] -- ^ extension fields
           -> AuthPlugin master
authOpenId idType extensionFields =
    AuthPlugin "openid" dispatch login
  where
    complete = PluginR "openid" ["complete"]

    name :: Text
    name = "openid_identifier"

    login tm = do
        ident <- newIdent
        -- FIXME this is a hack to get GHC 7.6's type checker to allow the
        -- code, but it shouldn't be necessary
        let y :: a -> [(Text, Text)] -> Text
            y = undefined
        toWidget (\x -> [cassius|##{ident}
    background: #fff url(http://www.myopenid.com/static/openid-icon-small.gif) no-repeat scroll 0pt 50%;
    padding-left: 18px;
|] $ x `asTypeOf` y)
        [whamlet|
$newline never
<form method="get" action="@{tm forwardUrl}">
    <input type="hidden" name="openid_identifier" value="http://me.yahoo.com">
    <button .openid-yahoo>_{Msg.LoginYahoo}
<form method="get" action="@{tm forwardUrl}">
    <label for="#{ident}">OpenID: #
    <input id="#{ident}" type="text" name="#{name}" value="http://">
    <input type="submit" value="_{Msg.LoginOpenID}">
|]

    dispatch :: Text -> [Text] -> AuthHandler master TypedContent
    dispatch "GET" ["forward"] = do
        roid <- runInputGet $ iopt textField name
        case roid of
            Just oid -> do
                tm <- getRouteToParent
                render <- getUrlRender
                let complete' = render $ tm complete
                manager <- authHttpManager
                eres <- tryAny $ OpenId.getForwardUrl oid complete' Nothing extensionFields manager
                case eres of
                    Left err -> loginErrorMessage (tm LoginR) $ T.pack $ show err
                    Right x -> redirect x
            Nothing -> loginErrorMessageI LoginR Msg.NoOpenID
    dispatch "GET" ["complete", ""] = dispatch "GET" ["complete"] -- compatibility issues
    dispatch "GET" ["complete"] = do
        rr <- getRequest
        completeHelper idType $ reqGetParams rr
    dispatch "POST" ["complete", ""] = dispatch "POST" ["complete"] -- compatibility issues
    dispatch "POST" ["complete"] = do
        (posts, _) <- runRequestBody
        completeHelper idType posts
    dispatch _ _ = notFound

completeHelper :: IdentifierType -> [(Text, Text)] -> AuthHandler master TypedContent
completeHelper idType gets' = do
    manager <- authHttpManager
    eres <- tryAny $ OpenId.authenticateClaimed gets' manager
    either onFailure onSuccess eres
  where
    onFailure err = do
        tm <- getRouteToParent
        loginErrorMessage (tm LoginR) $ T.pack $ show err
    onSuccess oir = do
            let claimed =
                    case OpenId.oirClaimed oir of
                        Nothing -> id
                        Just (OpenId.Identifier i') -> ((claimedKey, i'):)
                oplocal =
                    case OpenId.oirOpLocal oir of
                        OpenId.Identifier i' -> ((opLocalKey, i'):)
                gets'' = oplocal $ claimed $ filter (\(k, _) -> not $ "__" `isPrefixOf` k) gets'
                i = OpenId.identifier $
                        case idType of
                            OPLocal -> OpenId.oirOpLocal oir
                            Claimed -> fromMaybe (OpenId.oirOpLocal oir) $ OpenId.oirClaimed oir
            setCredsRedirect $ Creds "openid" i gets''

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

opLocalKey :: Text
opLocalKey = "__OPLOCAL"

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
