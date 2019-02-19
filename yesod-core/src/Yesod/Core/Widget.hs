{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Widgets combine HTML with JS and CSS dependencies with a unique identifier
-- generator, allowing you to create truly modular HTML components.
module Yesod.Core.Widget
    ( -- * Datatype
      WidgetT
    , WidgetFor
    , PageContent (..)
      -- * Special Hamlet quasiquoter/TH for Widgets
    , whamlet
    , whamletFile
    , ihamletToRepHtml
    , ihamletToHtml
      -- * Convert to Widget
    , ToWidget (..)
    , ToWidgetHead (..)
    , ToWidgetBody (..)
    , ToWidgetMedia (..)
      -- * Creating
      -- ** Head of page
    , setTitle
    , setTitleI
      -- ** CSS
    , addStylesheet
    , addStylesheetAttrs
    , addStylesheetRemote
    , addStylesheetRemoteAttrs
    , addStylesheetEither
    , CssBuilder (..)
      -- ** Javascript
    , addScript
    , addScriptAttrs
    , addScriptRemote
    , addScriptRemoteAttrs
    , addScriptEither
      -- * Subsites
    , handlerToWidget
      -- * Internal
    , whamletFileWithSettings
    , asWidgetT
    ) where

import Data.Monoid
import qualified Text.Blaze.Html5 as H
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Yesod.Routes.Class
import Yesod.Core.Handler (getMessageRender, getUrlRenderParams)
import Text.Shakespeare.I18N (RenderMessage)
import qualified RIO.Map as Map
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE, AppE), Pat (VarP), newName)

import qualified Text.Hamlet as NP
import Data.Text.Lazy.Builder (fromLazyText)
import Text.Blaze.Html (toHtml, preEscapedToMarkup)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

import RIO
import Yesod.Core.Types

type WidgetT site (m :: * -> *) = WidgetFor site
{-# DEPRECATED WidgetT "Use WidgetFor directly" #-}

preEscapedLazyText :: TL.Text -> Html
preEscapedLazyText = preEscapedToMarkup

class ToWidget site a where
    toWidget :: (HasWidgetData env, HandlerSite env ~ site) => a -> RIO env ()

instance render ~ RY site => ToWidget site (render -> Html) where
    toWidget x = tell $ GWData (Body x) mempty mempty mempty mempty mempty mempty
instance render ~ RY site => ToWidget site (render -> Css) where
    toWidget x = toWidget $ CssBuilder . fromLazyText . renderCss . x
instance ToWidget site Css where
    toWidget x = toWidget $ CssBuilder . fromLazyText . renderCss . const x
instance render ~ RY site => ToWidget site (render -> CssBuilder) where
    toWidget x = tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing $ unCssBuilder . x) mempty mempty
instance ToWidget site CssBuilder where
    toWidget x = tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing $ unCssBuilder . const x) mempty mempty
instance render ~ RY site => ToWidget site (render -> Javascript) where
    toWidget x = tell $ GWData mempty mempty mempty mempty mempty (Just x) mempty
instance ToWidget site Javascript where
    toWidget x = tell $ GWData mempty mempty mempty mempty mempty (Just $ const x) mempty
instance (site' ~ site, a ~ ()) => ToWidget site' (WidgetFor site a) where
    toWidget = liftWidget
instance ToWidget site Html where
    toWidget = toWidget . const
-- | @since 1.4.28
instance ToWidget site Text where
    toWidget = toWidget . toHtml
-- | @since 1.4.28
instance ToWidget site TL.Text where
    toWidget = toWidget . toHtml
-- | @since 1.4.28
instance ToWidget site TB.Builder where
    toWidget = toWidget . toHtml

-- | Allows adding some CSS to the page with a specific media type.
--
-- Since 1.2
class ToWidgetMedia site a where
    -- | Add the given content to the page, but only for the given media type.
    --
    -- Since 1.2
    toWidgetMedia :: (HasWidgetData env, HandlerSite env ~ site)
                  => Text -- ^ media value
                  -> a
                  -> RIO env ()
instance render ~ RY site => ToWidgetMedia site (render -> Css) where
    toWidgetMedia media x = toWidgetMedia media $ CssBuilder . fromLazyText . renderCss . x
instance ToWidgetMedia site Css where
    toWidgetMedia media x = toWidgetMedia media $ CssBuilder . fromLazyText . renderCss . const x
instance render ~ RY site => ToWidgetMedia site (render -> CssBuilder) where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . x) mempty mempty
instance ToWidgetMedia site CssBuilder where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . const x) mempty mempty

class ToWidgetBody site a where
    toWidgetBody :: (HasWidgetData env, HandlerSite env ~ site) => a -> RIO env ()

instance render ~ RY site => ToWidgetBody site (render -> Html) where
    toWidgetBody = toWidget
instance render ~ RY site => ToWidgetBody site (render -> Javascript) where
    toWidgetBody j = toWidget $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetBody site Javascript where
    toWidgetBody j = toWidget $ \_ -> H.script $ preEscapedLazyText $ renderJavascript j
instance ToWidgetBody site Html where
    toWidgetBody = toWidget

class ToWidgetHead site a where
    toWidgetHead :: (HasWidgetData env, HandlerSite env ~ site) => a -> RIO env ()

instance render ~ RY site => ToWidgetHead site (render -> Html) where
    toWidgetHead = tell . GWData mempty mempty mempty mempty mempty mempty . Head
instance render ~ RY site => ToWidgetHead site (render -> Css) where
    toWidgetHead = toWidget
instance ToWidgetHead site Css where
    toWidgetHead = toWidget
instance render ~ RY site => ToWidgetHead site (render -> CssBuilder) where
    toWidgetHead = toWidget
instance ToWidgetHead site CssBuilder where
    toWidgetHead = toWidget
instance render ~ RY site => ToWidgetHead site (render -> Javascript) where
    toWidgetHead j = toWidgetHead $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetHead site Javascript where
    toWidgetHead j = toWidgetHead $ \_ -> H.script $ preEscapedLazyText $ renderJavascript j
instance ToWidgetHead site Html where
    toWidgetHead = toWidgetHead . const

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitle :: HasWidgetData env => Html -> RIO env ()
setTitle x = tell $ GWData mempty (Last $ Just $ Title x) mempty mempty mempty mempty mempty

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitleI :: (HasWidgetData env, RenderMessage (HandlerSite env) msg) => msg -> RIO env ()
setTitleI msg = do
    mr <- getMessageRender
    setTitle $ toHtml $ mr msg

-- | Link to the specified local stylesheet.
addStylesheet :: HasWidgetData env => Route (HandlerSite env) -> RIO env ()
addStylesheet = flip addStylesheetAttrs []

-- | Link to the specified local stylesheet.
addStylesheetAttrs :: HasWidgetData env
                   => Route (HandlerSite env)
                   -> [(Text, Text)]
                   -> RIO env ()
addStylesheetAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Local x) y) mempty mempty mempty

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: HasWidgetData env => Text -> RIO env ()
addStylesheetRemote = flip addStylesheetRemoteAttrs []

-- | Link to the specified remote stylesheet.
addStylesheetRemoteAttrs :: HasWidgetData env => Text -> [(Text, Text)] -> RIO env ()
addStylesheetRemoteAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Remote x) y) mempty mempty mempty

addStylesheetEither :: HasWidgetData env
                    => Either (Route (HandlerSite env)) Text
                    -> RIO env ()
addStylesheetEither = either addStylesheet addStylesheetRemote

addScriptEither :: HasWidgetData env
                => Either (Route (HandlerSite env)) Text
                -> RIO env ()
addScriptEither = either addScript addScriptRemote

-- | Link to the specified local script.
addScript :: HasWidgetData env => Route (HandlerSite env) -> RIO env ()
addScript = flip addScriptAttrs []

-- | Link to the specified local script.
addScriptAttrs :: HasWidgetData env => Route (HandlerSite env) -> [(Text, Text)] -> RIO env ()
addScriptAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Local x) y) mempty mempty mempty mempty

-- | Link to the specified remote script.
addScriptRemote :: HasWidgetData env => Text -> RIO env ()
addScriptRemote = flip addScriptRemoteAttrs []

-- | Link to the specified remote script.
addScriptRemoteAttrs :: HasWidgetData env => Text -> [(Text, Text)] -> RIO env ()
addScriptRemoteAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Remote x) y) mempty mempty mempty mempty

whamlet :: QuasiQuoter
whamlet = NP.hamletWithSettings rules NP.defaultHamletSettings

whamletFile :: FilePath -> Q Exp
whamletFile = NP.hamletFileWithSettings rules NP.defaultHamletSettings

whamletFileWithSettings :: NP.HamletSettings -> FilePath -> Q Exp
whamletFileWithSettings = NP.hamletFileWithSettings rules

asWidgetT :: WidgetT site m () -> WidgetT site m ()
asWidgetT = id

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
                    (Just $ helper [|fmap (toHtml .) getMessageRender|])
            f env
    return $ NP.HamletRules ah ur $ \_ b -> return $ ah `AppE` b

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
ihamletToRepHtml :: (HasHandlerData env, RenderMessage (HandlerSite env) message)
                 => HtmlUrlI18n message (Route (HandlerSite env))
                 -> RIO env Html
ihamletToRepHtml = ihamletToHtml
{-# DEPRECATED ihamletToRepHtml "Please use ihamletToHtml instead" #-}

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
--
-- Since 1.2.1
ihamletToHtml :: (HasHandlerData env, RenderMessage (HandlerSite env) message)
              => HtmlUrlI18n message (Route (HandlerSite env))
              -> RIO env Html
ihamletToHtml ih = do
    urender <- getUrlRenderParams
    mrender <- getMessageRender
    return $ ih (toHtml . mrender) urender

tell :: HasWidgetData env => GWData (Route (HandlerSite env)) -> RIO env ()
tell = liftWidget . tellWidget

toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)

handlerToWidget :: HandlerFor site a -> WidgetFor site a
handlerToWidget = liftHandler
