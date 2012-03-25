{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.BrowserId
    ( authBrowserId
    , authBrowserIdAudience
    ) where

import Yesod.Auth
import Web.Authenticate.BrowserId
import Data.Text (Text)
import Yesod.Core
import Text.Hamlet (hamlet)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)

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
                master <- getYesod
                audience <-
                    case maudience of
                        Just a -> return a
                        Nothing -> do
                            tm <- getRouteToMaster
                            r <- getUrlRender
                            return $ T.takeWhile (/= '/') $ stripScheme $ r $ tm LoginR
                memail <- lift $ checkAssertion audience assertion (authHttpManager master)
                case memail of
                    Nothing -> liftIO $ throwIO InvalidBrowserIDAssertion
                    Just email -> setCreds True Creds
                        { credsPlugin = pid
                        , credsIdent = email
                        , credsExtra = []
                        }
            (_, []) -> badMethod
            _ -> notFound
    , apLogin = \toMaster -> do
        addScriptRemote browserIdJs
        toWidget [hamlet|
<p>
    <a href="javascript:navigator.id.getVerifiedEmail(function(a){if(a)document.location='@{toMaster complete}/'+a});">
        <img src="https://browserid.org/i/sign_in_green.png">
|]
    }
  where
    stripScheme t = fromMaybe t $ T.stripPrefix "//" $ snd $ T.breakOn "//" t
