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
{-# LANGUAGE CPP #-}
-- | Widgets combine HTML with JS and CSS dependencies with a unique identifier
-- generator, allowing you to create truly modular HTML components.
module Yesod.Core.Widget
    ( -- * Datatype
      WidgetT
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
    , widgetToParentWidget
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Shakespeare.I18N (RenderMessage)
import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Map as Map
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE, AppE), Pat (VarP), newName)

import qualified Text.Hamlet as NP
import Data.Text.Lazy.Builder (fromLazyText)
import Text.Blaze.Html (toHtml, preEscapedToMarkup)
import qualified Data.Text.Lazy as TL

import Yesod.Core.Types
import Yesod.Core.Class.Handler

preEscapedLazyText :: TL.Text -> Html
preEscapedLazyText = preEscapedToMarkup

class ToWidget site a where
    toWidget :: (MonadWidget m, HandlerSite m ~ site) => a -> m ()

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
instance (site' ~ site, IO ~ m, a ~ ()) => ToWidget site' (WidgetT site m a) where
    toWidget = liftWidgetT
instance ToWidget site Html where
    toWidget = toWidget . const

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
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . x) mempty mempty
instance ToWidgetMedia site CssBuilder where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . const x) mempty mempty

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
setTitle :: MonadWidget m => Html -> m ()
setTitle x = tell $ GWData mempty (Last $ Just $ Title x) mempty mempty mempty mempty mempty

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitleI :: (MonadWidget m, RenderMessage (HandlerSite m) msg) => msg -> m ()
setTitleI msg = do
    mr <- getMessageRender
    setTitle $ toHtml $ mr msg

-- | Link to the specified local stylesheet.
addStylesheet :: MonadWidget m => Route (HandlerSite m) -> m ()
addStylesheet = flip addStylesheetAttrs []

-- | Link to the specified local stylesheet.
addStylesheetAttrs :: MonadWidget m
                   => Route (HandlerSite m)
                   -> [(Text, Text)]
                   -> m ()
addStylesheetAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Local x) y) mempty mempty mempty

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: MonadWidget m => Text -> m ()
addStylesheetRemote = flip addStylesheetRemoteAttrs []

-- | Link to the specified remote stylesheet.
addStylesheetRemoteAttrs :: MonadWidget m => Text -> [(Text, Text)] -> m ()
addStylesheetRemoteAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Remote x) y) mempty mempty mempty

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
addScriptAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Local x) y) mempty mempty mempty mempty

-- | Link to the specified remote script.
addScriptRemote :: MonadWidget m => Text -> m ()
addScriptRemote = flip addScriptRemoteAttrs []

-- | Link to the specified remote script.
addScriptRemoteAttrs :: MonadWidget m => Text -> [(Text, Text)] -> m ()
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

tell :: MonadWidget m => GWData (Route (HandlerSite m)) -> m ()
tell w = liftWidgetT $ WidgetT $ const $ return ((), w)

toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)

handlerToWidget :: Monad m => HandlerT site m a -> WidgetT site m a
handlerToWidget (HandlerT f) = WidgetT $ liftM (, mempty) . f

widgetToParentWidget :: MonadIO m
                     => WidgetT child IO a
                     -> HandlerT child (HandlerT parent m) (WidgetT parent m a)
widgetToParentWidget (WidgetT f) = HandlerT $ \hd -> do
    (a, gwd) <- liftIO $ f hd { handlerToParent = const () }
    return $ WidgetT $ const $ return (a, liftGWD (handlerToParent hd) gwd)

liftGWD :: (child -> parent) -> GWData child -> GWData parent
liftGWD tp gwd = GWData
    { gwdBody = fixBody $ gwdBody gwd
    , gwdTitle = gwdTitle gwd
    , gwdScripts = fixUnique fixScript $ gwdScripts gwd
    , gwdStylesheets = fixUnique fixStyle $ gwdStylesheets gwd
    , gwdCss = fmap fixCss $ gwdCss gwd
    , gwdJavascript = fmap fixJS $ gwdJavascript gwd
    , gwdHead = fixHead $ gwdHead gwd
    }
  where
    fixRender f route params = f (tp route) params

    fixBody (Body h) = Body $ h . fixRender
    fixHead (Head h) = Head $ h . fixRender

    fixUnique go (UniqueList f) = UniqueList (map go (f []) ++)

    fixScript (Script loc attrs) = Script (fixLoc loc) attrs
    fixStyle (Stylesheet loc attrs) = Stylesheet (fixLoc loc) attrs

    fixLoc (Local url) = Local $ tp url
    fixLoc (Remote t) = Remote t

    fixCss f = f . fixRender

    fixJS f = f . fixRender
