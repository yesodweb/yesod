{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.BrowserId
    ( authBrowserId
    , authBrowserId'
    , authBrowserIdAudience
    ) where

import Yesod.Auth
import Web.Authenticate.BrowserId
import Data.Text (Text)
import Yesod.Core
import Text.Hamlet (hamlet)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

#include "qq.h"

pid :: Text
pid = "browserid"

complete :: AuthRoute
complete = PluginR pid []

authBrowserIdAudience :: YesodAuth m
              => Text -- ^ audience
              -> AuthPlugin m
authBrowserIdAudience audience = AuthPlugin
    { apName = pid
    , apDispatch = \m ps ->
        case (m, ps) of
            ("GET", [assertion]) -> do
                memail <- liftIO $ checkAssertion audience assertion
                case memail of
                    Nothing -> error "Invalid assertion"
                    Just email -> setCreds True Creds
                        { credsPlugin = pid
                        , credsIdent = email
                        , credsExtra = []
                        }
            (_, []) -> badMethod
            _ -> notFound
    , apLogin = \toMaster -> do
        addScriptRemote browserIdJs
        addHamlet [QQ(hamlet)|
<p>
    <a href="javascript:navigator.id.getVerifiedEmail(function(a){if(a)document.location='@{toMaster complete}/'+a});">
        <img src="https://browserid.org/i/sign_in_green.png">
|]
    }

authBrowserId :: YesodAuth m => AuthPlugin m
authBrowserId = AuthPlugin
    { apName = pid
    , apDispatch = \m ps ->
        case (m, ps) of
            ("GET", [assertion]) -> do
                tm <- getRouteToMaster
                r <- getUrlRender
                let audience = T.takeWhile (/= '/') $ stripScheme $ r $ tm LoginR
                memail <- liftIO $ checkAssertion audience assertion
                case memail of
                    Nothing -> error "Invalid assertion"
                    Just email -> setCreds True Creds
                        { credsPlugin = pid
                        , credsIdent = email
                        , credsExtra = []
                        }
            (_, []) -> badMethod
            _ -> notFound
    , apLogin = \toMaster -> do
        addScriptRemote browserIdJs
        addHamlet [QQ(hamlet)|
<p>
    <a href="javascript:navigator.id.getVerifiedEmail(function(a){if(a)document.location='@{toMaster complete}/'+a});">
        <img src="https://browserid.org/i/sign_in_green.png">
|]
    }
  where
    stripScheme t = fromMaybe t $ T.stripPrefix "//" $ snd $ T.breakOn "//" t

authBrowserId' :: YesodAuth m => AuthPlugin m
authBrowserId' = authBrowserId
{-# DEPRECATED authBrowserId' "Use authBrowserId instead" #-}
