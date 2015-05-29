{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings                   #-}
{-# LANGUAGE QuasiQuotes                         #-}
{-# LANGUAGE TypeFamilies                        #-}
module Yesod.Shakespeare (
      whamlet
    , whamletFile
      -- * Special Hamlet quasiquoter/TH for Widgets
    , ihamletToRepHtml
    , ihamletToHtml
      -- * Internal
    , whamletFileWithSettings
      -- * Creating
      -- ** Head of page
    , setTitleI
      -- ** Errors
    , permissionDeniedI
    , invalidArgsI
    , unauthorizedI
      -- ** Messages
    , setMessageI
      -- * i18n
    , getMessageRender

      -- * Formerly Yesod.Core.Handler
      -- ** Redirecting
    , redirectToPost

      -- * Shakespeare
      -- ** Hamlet
    , hamlet
    , shamlet
    , xhamlet
    , HtmlUrl
      -- ** Julius
    , julius
    , JavascriptUrl
    , renderJavascriptUrl
      -- ** Cassius/Lucius
    , cassius
    , lucius
    , CssUrl
    , renderCssUrl

    , module Text.Shakespeare.I18N
) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad                      (liftM, forM)
import           Control.Monad.Trans.Class (lift)
import Text.Shakespeare.I18N
import qualified Data.ByteString.Lazy               as L
import           Data.List                          (foldl', nub)
import           Text.Blaze.Html               (preEscapedToMarkup, toHtml, Html)
import qualified Text.Blaze.Html5 as H
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE, AppE), Pat (VarP), newName)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (fromLazyText, toLazyText)
import Data.Monoid (Last(..), mempty)
import qualified Data.Map as Map

import qualified Text.Hamlet as NP
import Text.Julius (Javascript(..), JavascriptUrl, renderJavascript, renderJavascriptUrl, julius)
import Text.Hamlet (hamlet, shamlet, xhamlet)
import Text.Lucius (Css, renderCss, CssUrl, renderCssUrl, lucius)
import Text.Cassius (cassius)

import Yesod.Core.Types
import Yesod.Core.Widget
import Yesod.Core.Class.Handler (HandlerSite, MonadHandler)
import Yesod.Core.Handler (getUrlRenderParams, toTextUrl, invalidArgs, permissionDenied, RedirectUrl, withUrlRenderer, getRequest, getYesod, sendResponse)
import Yesod.Core.Content (ToContent(..), ToTypedContent(..), HasContentType(..), typeJavascript, typeCss)
import           Yesod.Routes.Class            (Route)

-- for hamlet expansion
import qualified Data.Foldable
import qualified Data.Text
import Text.Hamlet (asHtmlUrl)

type Translate msg = msg -> Html
type HtmlUrlI18n msg url = Translate msg -> Render url -> Html
type Render url = url -> [(Text, Text)] -> Text
type HtmlUrl url = Render url -> Html

preEscapedLazyText :: TL.Text -> Html
preEscapedLazyText = preEscapedToMarkup

whamlet :: QuasiQuoter
whamlet = NP.hamletWithSettings rules NP.defaultHamletSettings

whamletFile :: FilePath -> Q Exp
whamletFile = NP.hamletFileWithSettings rules NP.defaultHamletSettings

whamletFileWithSettings :: NP.HamletSettings -> FilePath -> Q Exp
whamletFileWithSettings = NP.hamletFileWithSettings rules

rules :: Q NP.HamletRules
rules = do
    ah <- [|asWidgetT . toWidget|]
    let helper qg f = do
            x <- newName "urender"
            e <- f $ VarE x
            let e' = LamE [VarP x] e
            g <- qg
            bind <- [|(>>=)|]
            return $ InfixE (Just g) bind (Just e')
    let ur f = do
            let env = NP.Env
                    (Just $ helper [|getUrlRenderParams|])
                    (Just $ helper [|liftM (toHtml .) getMessageRender|])
            f env
    return $ NP.HamletRules ah ur $ \_ b -> return $ ah `AppE` b

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
ihamletToRepHtml :: (MonadHandler m, RenderMessage (HandlerSite m) message)
                 => HtmlUrlI18n message (Route (HandlerSite m))
                 -> m Html
ihamletToRepHtml = ihamletToHtml
{-# DEPRECATED ihamletToRepHtml "Please use ihamletToHtml instead" #-}

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
--
-- Since 1.2.1
ihamletToHtml :: (MonadHandler m, RenderMessage (HandlerSite m) message)
              => HtmlUrlI18n message (Route (HandlerSite m))
              -> m Html
ihamletToHtml ih = do
    urender <- getUrlRenderParams
    mrender <- getMessageRender
    return $ ih (toHtml . mrender) urender


-- | Redirect to a POST resource.
--
-- This is not technically a redirect; instead, it returns an HTML page with a
-- POST form, and some Javascript to automatically submit the form. This can be
-- useful when you need to post a plain link somewhere that needs to cause
-- changes on the server.
redirectToPost :: (MonadHandler m, RedirectUrl (HandlerSite m) url)
               => url
               -> m a
redirectToPost url = do
    urlText <- toTextUrl url
    withUrlRenderer [hamlet|
$newline never
$doctype 5

<html>
    <head>
        <title>Redirecting...
    <body onload="document.getElementById('form').submit()">
        <form id="form" method="post" action=#{urlText}>
            <noscript>
                <p>Javascript has been disabled; please click on the button below to be redirected.
            <input type="submit" value="Continue">
|] >>= sendResponse

------------------------------------
-- Formerly Yesod.Core.Content
------------------------------------
instance ToContent Css where
    toContent = toContent . renderCss
instance ToContent Javascript where
    toContent = toContent . toLazyText . unJavascript
instance HasContentType Css where
    getContentType _ = typeCss
instance HasContentType Javascript where
    getContentType _ = typeJavascript
instance ToTypedContent Css where
    toTypedContent = TypedContent typeCss . toContent
instance ToTypedContent Javascript where
    toTypedContent = TypedContent typeJavascript . toContent

------------------------------------
-- Formerly Yesod.Core.Widget
------------------------------------
instance render ~ RY site => ToWidget site (render -> Css) where
    toWidget x = toWidget $ CssBuilder . fromLazyText . renderCss . x
instance ToWidget site Css where
    toWidget x = toWidget $ CssBuilder . fromLazyText . renderCss . const x
instance render ~ RY site => ToWidget site (render -> Javascript) where
    toWidget x = tellWidget $ GWData mempty mempty mempty mempty mempty (Just $ unJavascript . x) mempty
instance ToWidget site Javascript where
    toWidget x = tellWidget $ GWData mempty mempty mempty mempty mempty (Just $ const $ unJavascript x) mempty
instance render ~ RY site => ToWidgetMedia site (render -> Css) where
    toWidgetMedia media x = toWidgetMedia media $ CssBuilder . fromLazyText . renderCss . x
instance ToWidgetMedia site Css where
    toWidgetMedia media x = toWidgetMedia media $ CssBuilder . fromLazyText . renderCss . const x
instance render ~ RY site => ToWidgetHead site (render -> Css) where
    toWidgetHead = toWidget
instance render ~ RY site => ToWidgetHead site (render -> Javascript) where
    toWidgetHead j = toWidgetHead $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetHead site Javascript where
    toWidgetHead j = toWidgetHead $ \_ -> H.script $ preEscapedLazyText $ renderJavascript j
instance render ~ RY site => ToWidgetBody site (render -> Javascript) where
    toWidgetBody j = toWidget $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetBody site Javascript where
    toWidgetBody j = toWidget $ \_ -> H.script $ preEscapedLazyText $ renderJavascript j
instance ToWidgetHead site Html where
    toWidgetHead = toWidgetHead . const
instance ToWidgetHead site Css where
    toWidgetHead = toWidget
instance render ~ RY site => ToWidgetHead site (render -> CssBuilder) where
    toWidgetHead = toWidget
instance ToWidgetHead site CssBuilder where
    toWidgetHead = toWidget

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitleI :: (MonadWidget m, RenderMessage (HandlerSite m) msg) => msg -> m ()
setTitleI msg = do
    mr <- getMessageRender
    setTitle $ toHtml $ mr msg


-- | Sets a message in the user's session.
--
-- See 'getMessage'.
setMessageI :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
            => msg -> m ()
setMessageI msg = do
    mr <- getMessageRender
    setMessage $ toHtml $ mr msg

-- | Return a 403 permission denied page.
permissionDeniedI :: (RenderMessage (HandlerSite m) msg, MonadHandler m)
                  => msg
                  -> m a
permissionDeniedI msg = do
    mr <- getMessageRender
    permissionDenied $ mr msg

-- | Return a 400 invalid arguments page.
invalidArgsI :: (MonadHandler m, RenderMessage (HandlerSite m) msg) => [msg] -> m a
invalidArgsI msg = do
    mr <- getMessageRender
    invalidArgs $ map mr msg


getMessageRender :: (MonadHandler m, RenderMessage (HandlerSite m) message)
                 => m (message -> Text)
getMessageRender = do
    site <- getYesod
    l <- reqLangs `liftM` getRequest
    return $ renderMessage site l

-----------------------------
-- originally from Yesod.Core
-----------------------------
-- | Return an 'Unauthorized' value, with the given i18n message.
unauthorizedI :: (MonadHandler m, RenderMessage (HandlerSite m) msg) => msg -> m AuthResult
unauthorizedI msg = do
    mr <- getMessageRender
    return $ Unauthorized $ mr msg

