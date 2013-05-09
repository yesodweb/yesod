{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Yesod.Auth.BrowserId
    ( authBrowserId
    , createOnClick
    , def
    , BrowserIdSettings
    , bisAudience
    , bisLazyLoad
    ) where

import Yesod.Auth
import Web.Authenticate.BrowserId
import Data.Text (Text)
import Yesod.Core
import Text.Hamlet (hamlet)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless)
import Text.Julius (julius, rawJS)
import Network.URI (uriPath, parseURI)
import Data.FileEmbed (embedFile)
import Data.ByteString (ByteString)
import Data.Default

pid :: Text
pid = "browserid"

complete :: Route Auth
complete = PluginR pid []

-- | A settings type for various configuration options relevant to BrowserID.
--
-- See: <http://www.yesodweb.com/book/settings-types>
--
-- Since 1.2.0
data BrowserIdSettings = BrowserIdSettings
    { bisAudience :: Maybe Text
    -- ^ BrowserID audience value. If @Nothing@, will be extracted based on the
    -- approot.
    --
    -- Default: @Nothing@
    --
    -- Since 1.2.0
    , bisLazyLoad :: Bool
    -- ^ Use asynchronous Javascript loading for the BrowserID JS file.
    --
    -- Default: @True@.
    --
    -- Since 1.2.0
    }

instance Default BrowserIdSettings where
    def = BrowserIdSettings
        { bisAudience = Nothing
        , bisLazyLoad = True
        }

authBrowserId :: YesodAuth m => BrowserIdSettings -> AuthPlugin m
authBrowserId bis@BrowserIdSettings {..} = AuthPlugin
    { apName = pid
    , apDispatch = \m ps ->
        case (m, ps) of
            ("GET", [assertion]) -> do
                master <- lift getYesod
                audience <-
                    case bisAudience of
                        Just a -> return a
                        Nothing -> do
                            r <- getUrlRender
                            return $ T.takeWhile (/= '/') $ stripScheme $ r LoginR
                memail <- lift $ checkAssertion audience assertion (authHttpManager master)
                case memail of
                    Nothing -> do
                      $logErrorS "yesod-auth" "BrowserID assertion failure"
                      loginErrorMessage LoginR "BrowserID login error."
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
        onclick <- createOnClick bis toMaster

        autologin <- fmap (== Just "true") $ lookupGetParam "autologin"
        when autologin $ toWidget [julius|#{rawJS onclick}();|]

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
createOnClick :: BrowserIdSettings
              -> (Route Auth -> Route master)
              -> WidgetT master IO Text
createOnClick BrowserIdSettings {..} toMaster = do
    unless bisLazyLoad $ addScriptRemote browserIdJs
    onclick <- newIdent
    render <- getUrlRender
    let login = toJSON $ getPath $ render (toMaster LoginR)
    toWidget [julius|
        function #{rawJS onclick}() {
            if (navigator.id) {
                navigator.id.watch({
                    onlogin: function (assertion) {
                        if (assertion) {
                            document.location = "@{toMaster complete}/" + assertion;
                        }
                    },
                    onlogout: function () {}
                });
                navigator.id.request({
                    returnTo: #{login} + "?autologin=true"
                });
            }
            else {
                alert("Loading, please try again");
            }
        }
    |]
    when bisLazyLoad $ toWidget [julius|
        (function(){
            var bid = document.createElement("script");
            bid.async = true;
            bid.src = #{toJSON browserIdJs};
            var s = document.getElementsByTagName('script')[0];
            s.parentNode.insertBefore(bid, s);
        })();
    |]

    autologin <- fmap (== Just "true") $ lookupGetParam "autologin"
    when autologin $ toWidget [julius|#{rawJS onclick}();|]
    return onclick
  where
    getPath t = fromMaybe t $ do
        uri <- parseURI $ T.unpack t
        return $ T.pack $ uriPath uri
