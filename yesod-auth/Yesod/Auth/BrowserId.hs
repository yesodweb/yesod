{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Auth.BrowserId
    ( authBrowserId
    , authBrowserIdAudience
    , createOnClick
    ) where

import Yesod.Auth
import Web.Authenticate.BrowserId
import Data.Text (Text)
import Yesod.Core
import Text.Hamlet (hamlet)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Control.Exception (throwIO)
import Text.Julius (julius, rawJS)
import Network.URI (uriPath, parseURI)
import Data.FileEmbed (embedFile)
import Data.ByteString (ByteString)

pid :: Text
pid = "browserid"

complete :: Route Auth
complete = PluginR pid []

-- | Log into browser ID with an audience value determined from the 'approot'.
authBrowserId :: YesodAuth m => AuthPlugin m
authBrowserId = helper Nothing

-- | Log into browser ID with the given audience value. Note that this must be
-- your actual hostname, or login will fail.
authBrowserIdAudience
    :: YesodAuth m
    => Text -- ^ audience
    -> AuthPlugin m
authBrowserIdAudience = helper . Just

helper :: YesodAuth m
       => Maybe Text -- ^ audience
       -> AuthPlugin m
helper maudience = AuthPlugin
    { apName = pid
    , apDispatch = \m ps ->
        case (m, ps) of
            ("GET", [assertion]) -> do
                master <- lift getYesod
                audience <-
                    case maudience of
                        Just a -> return a
                        Nothing -> do
                            r <- getUrlRender
                            return $ T.takeWhile (/= '/') $ stripScheme $ r LoginR
                memail <- lift $ checkAssertion audience assertion (authHttpManager master)
                case memail of
                    Nothing -> liftIO $ throwIO InvalidBrowserIDAssertion
                    Just email -> lift $ setCreds True Creds
                        { credsPlugin = pid
                        , credsIdent = email
                        , credsExtra = []
                        }
            ("GET", ["static", "sign-in.png"]) -> sendResponse
                ( "image/png" :: ByteString
                , toContent $(embedFile "persona_sign_in_blue.png")
                )
            (_, []) -> badMethod
            _ -> notFound
    , apLogin = \toMaster -> do
        onclick <- createOnClick toMaster

        autologin <- fmap (== Just "true") $ lookupGetParam "autologin"
        when autologin $ toWidget [julius|
#{rawJS onclick}();
|]

        toWidget [hamlet|
$newline never
<p>
    <a href="javascript:#{onclick}()">
        <img src=@{toMaster loginIcon}>
|]
    }
  where
    loginIcon = PluginR pid ["static", "sign-in.png"]
    stripScheme t = fromMaybe t $ T.stripPrefix "//" $ snd $ T.breakOn "//" t

-- | Generates a function to handle on-click events, and returns that function
-- name.
createOnClick :: (Route Auth -> Route master) -> WidgetT master IO Text
createOnClick toMaster = do
    addScriptRemote browserIdJs
    onclick <- newIdent
    render <- getUrlRender
    let login = toJSON $ getPath $ render $ toMaster LoginR
    toWidget [julius|
        function #{rawJS onclick}() {
            navigator.id.watch({
                onlogin: function (assertion) {
                    if (assertion) {
                        document.location = "@{toMaster complete}" + "/" + assertion;
                    }
                },
                onlogout: function () {}
            });
            navigator.id.request({
                returnTo: #{login} + "?autologin=true"
            });
        }
    |]

    autologin <- fmap (== Just "true") $ lookupGetParam "autologin"
    when autologin $ toWidget [julius|#{rawJS onclick}();|]
    return onclick
  where
    getPath t = fromMaybe t $ do
        uri <- parseURI $ T.unpack t
        return $ T.pack $ uriPath uri
