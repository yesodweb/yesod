{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

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
    , setDescription
    , setDescriptionI
    , setDescriptionIdemp
    , setDescriptionIdempI
    , setOGType
    , setOGImage
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
import Data.Text (Text)
import Data.Kind (Type)
import qualified Data.Map as Map
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE, AppE), Pat (VarP), newName)

import qualified Text.Hamlet as NP
import Data.Text.Lazy.Builder (fromLazyText)
import Text.Blaze.Html (toHtml, preEscapedToMarkup)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

import Yesod.Core.Types
import Yesod.Core.Class.Handler

type WidgetT site (m :: Type -> Type) = WidgetFor site
{-# DEPRECATED WidgetT "Use WidgetFor directly" #-}

preEscapedLazyText :: TL.Text -> Html
preEscapedLazyText = preEscapedToMarkup

class ToWidget site a where
    toWidget :: (MonadWidget m, HandlerSite m ~ site) => a -> m ()

instance render ~ RY site => ToWidget site (render -> Html) where
    toWidget x = tell $ GWData (Body x) mempty mempty mempty mempty mempty mempty mempty
instance render ~ RY site => ToWidget site (render -> Css) where
    toWidget x = toWidget $ CssBuilder . fromLazyText . renderCss . x
instance ToWidget site Css where
    toWidget x = toWidget $ CssBuilder . fromLazyText . renderCss . const x
instance render ~ RY site => ToWidget site (render -> CssBuilder) where
    toWidget x = tell $ GWData mempty mempty mempty mempty mempty (Map.singleton Nothing $ unCssBuilder . x) mempty mempty
instance ToWidget site CssBuilder where
    toWidget x = tell $ GWData mempty mempty mempty mempty mempty (Map.singleton Nothing $ unCssBuilder . const x) mempty mempty
instance render ~ RY site => ToWidget site (render -> Javascript) where
    toWidget x = tell $ GWData mempty mempty mempty mempty mempty mempty (Just x) mempty
instance ToWidget site Javascript where
    toWidget x = tell $ GWData mempty mempty mempty mempty mempty mempty (Just $ const x) mempty
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
    toWidgetMedia :: (MonadWidget m, HandlerSite m ~ site)
                  => Text -- ^ media value
                  -> a
                  -> m ()
instance render ~ RY site => ToWidgetMedia site (render -> Css) where
    toWidgetMedia media x = toWidgetMedia media $ CssBuilder . fromLazyText . renderCss . x
instance ToWidgetMedia site Css where
    toWidgetMedia media x = toWidgetMedia media $ CssBuilder . fromLazyText . renderCss . const x
instance render ~ RY site => ToWidgetMedia site (render -> CssBuilder) where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . x) mempty mempty
instance ToWidgetMedia site CssBuilder where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . const x) mempty mempty

class ToWidgetBody site a where
    toWidgetBody :: (MonadWidget m, HandlerSite m ~ site) => a -> m ()

instance render ~ RY site => ToWidgetBody site (render -> Html) where
    toWidgetBody = toWidget
instance render ~ RY site => ToWidgetBody site (render -> Javascript) where
    toWidgetBody j = toWidget $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetBody site Javascript where
    toWidgetBody j = toWidget $ \_ -> H.script $ preEscapedLazyText $ renderJavascript j
instance ToWidgetBody site Html where
    toWidgetBody = toWidget

class ToWidgetHead site a where
    toWidgetHead :: (MonadWidget m, HandlerSite m ~ site) => a -> m ()

instance render ~ RY site => ToWidgetHead site (render -> Html) where
    toWidgetHead = tell . GWData mempty mempty mempty mempty mempty mempty mempty . Head
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

-- | Set the page title.
--
-- Calling @setTitle@ or @setTitleI@ multiple times overrides previously set
-- values.
--
-- SEO Notes:
--
--    * Title tags are the second most important on-page factor for SEO, after
--      content
--    * Every page should have a unique title tag
--    * Start your title tag with your main targeted keyword
--    * Don't stuff your keywords
--    * Google typically shows 55-64 characters, so aim to keep your title
--      length under 60 characters
setTitle :: MonadWidget m => Html -> m ()
setTitle x = tell $ GWData mempty (Last $ Just $ Title x) mempty mempty mempty mempty mempty mempty

-- | Set the localised page title.
--
-- n.b. See comments for @setTitle@
setTitleI :: (MonadWidget m, RenderMessage (HandlerSite m) msg) => msg -> m ()
setTitleI msg = do
    mr <- getMessageRender
    setTitle $ toHtml $ mr msg

-- | Add description meta tag to the head of the page
--
-- Google does not use the description tag as a ranking signal, but the
-- contents of this tag will likely affect your click-through rate since it
-- shows up in search results.
--
-- The average length of the description shown in Google's search results is
-- about 160 characters on desktop, and about 130 characters on mobile, at time
-- of writing.
--
-- Source: https://www.advancedwebranking.com/blog/meta-tags-important-in-seo/
--
-- @since 1.6.18
setDescription :: MonadWidget m => Text -> m ()
setDescription description =
    toWidgetHead $ [hamlet|<meta name=description content=#{description}>|]

{-# WARNING setDescription
  [ "setDescription is not idempotent; we recommend setDescriptionIdemp instead"
  , "Multiple calls to setDescription will insert multiple meta tags in the page head."
  , "If you want an idempotent function, use setDescriptionIdemp - but if you do, you \
    \may need to change your layout to include pageDescription."
  ]
#-}

-- | Add translated description meta tag to the head of the page
--
-- n.b. See comments for @setDescription@.
--
-- @since 1.6.18
setDescriptionI
  :: (MonadWidget m, RenderMessage (HandlerSite m) msg)
  => msg -> m ()
setDescriptionI msg = do
    mr <- getMessageRender
    toWidgetHead $ [hamlet|<meta name=description content=#{mr msg}>|]

{-# WARNING setDescriptionI
  [ "setDescriptionI is not idempotent; we recommend setDescriptionIdempI instead"
  , "Multiple calls to setDescriptionI will insert multiple meta tags in the page head."
  , "If you want an idempotent function, use setDescriptionIdempI - but if you do, you \
    \may need to change your layout to include pageDescription."
  ]
#-}

-- | Add description meta tag to the head of the page
--
-- Google does not use the description tag as a ranking signal, but the
-- contents of this tag will likely affect your click-through rate since it
-- shows up in search results.
--
-- The average length of the description shown in Google's search results is
-- about 160 characters on desktop, and about 130 characters on mobile, at time
-- of writing.
--
-- Unlike 'setDescription', this version is *idempotent* - calling it multiple
-- times will result in only a single description meta tag in the head.
--
-- Source: https://www.advancedwebranking.com/blog/meta-tags-important-in-seo/
--
-- @since 1.6.23
setDescriptionIdemp :: MonadWidget m => Text -> m ()
setDescriptionIdemp description = tell $ GWData mempty mempty (Last $ Just $ Description description) mempty mempty mempty mempty mempty

-- | Add translated description meta tag to the head of the page
--
-- n.b. See comments for @setDescriptionIdemp@.
--
-- Unlike 'setDescriptionI', this version is *idempotent* - calling it multiple
-- times will result in only a single description meta tag in the head.
--
-- @since 1.6.23
setDescriptionIdempI
  :: (MonadWidget m, RenderMessage (HandlerSite m) msg)
  => msg -> m ()
setDescriptionIdempI msg = do
    mr <- getMessageRender
    setDescriptionIdemp $ mr msg

-- | Add OpenGraph type meta tag to the head of the page
--
-- See all available OG types here: https://ogp.me/#types
--
-- @since 1.6.18
setOGType :: MonadWidget m => Text -> m ()
setOGType a = toWidgetHead $ [hamlet|<meta property="og:type" content=#{a}>|]

-- | Add OpenGraph image meta tag to the head of the page
--
-- Best practices:
--
--    * Use custom images for shareable pages, e.g., homepage, articles, etc.
--    * Use your logo or any other branded image for the rest of your pages.
--    * Use images with a 1.91:1 ratio and minimum recommended dimensions of
--      1200x630 for optimal clarity across all devices.
--
-- Source: https://ahrefs.com/blog/open-graph-meta-tags/
--
-- @since 1.6.18
setOGImage :: MonadWidget m => Text -> m ()
setOGImage a = toWidgetHead $ [hamlet|<meta property="og:image" content=#{a}>|]

-- | Link to the specified local stylesheet.
addStylesheet :: MonadWidget m => Route (HandlerSite m) -> m ()
addStylesheet = flip addStylesheetAttrs []

-- | Link to the specified local stylesheet.
addStylesheetAttrs :: MonadWidget m
                   => Route (HandlerSite m)
                   -> [(Text, Text)]
                   -> m ()
addStylesheetAttrs x y = tell $ GWData mempty mempty mempty mempty (toUnique $ Stylesheet (Local x) y) mempty mempty mempty

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: MonadWidget m => Text -> m ()
addStylesheetRemote = flip addStylesheetRemoteAttrs []

-- | Link to the specified remote stylesheet.
addStylesheetRemoteAttrs :: MonadWidget m => Text -> [(Text, Text)] -> m ()
addStylesheetRemoteAttrs x y = tell $ GWData mempty mempty mempty mempty (toUnique $ Stylesheet (Remote x) y) mempty mempty mempty

addStylesheetEither :: MonadWidget m
                    => Either (Route (HandlerSite m)) Text
                    -> m ()
addStylesheetEither = either addStylesheet addStylesheetRemote

addScriptEither :: MonadWidget m
                => Either (Route (HandlerSite m)) Text
                -> m ()
addScriptEither = either addScript addScriptRemote

-- | Link to the specified local script.
addScript :: MonadWidget m => Route (HandlerSite m) -> m ()
addScript = flip addScriptAttrs []

-- | Link to the specified local script.
addScriptAttrs :: MonadWidget m => Route (HandlerSite m) -> [(Text, Text)] -> m ()
addScriptAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Script (Local x) y) mempty mempty mempty mempty

-- | Link to the specified remote script.
addScriptRemote :: MonadWidget m => Text -> m ()
addScriptRemote = flip addScriptRemoteAttrs []

-- | Link to the specified remote script.
addScriptRemoteAttrs :: MonadWidget m => Text -> [(Text, Text)] -> m ()
addScriptRemoteAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Script (Remote x) y) mempty mempty mempty mempty

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

tell :: MonadWidget m => GWData (Route (HandlerSite m)) -> m ()
tell = liftWidget . tellWidget

toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)

handlerToWidget :: HandlerFor site a -> WidgetFor site a
handlerToWidget (HandlerFor f) = WidgetFor $ f . wdHandler
