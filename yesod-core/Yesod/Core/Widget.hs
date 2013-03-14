{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , liftWidget
      -- * Internal
    , whamletFileWithSettings
    ) where

import Data.Monoid
import qualified Text.Blaze.Html5 as H
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Yesod.Routes.Class
import Yesod.Core.Handler (getMessageRender, getUrlRenderParams)
import Control.Monad.Trans.Resource (transResourceT)
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
import Control.Monad.Trans.Class

preEscapedLazyText :: TL.Text -> Html
preEscapedLazyText = preEscapedToMarkup

class Monad m => ToWidget site m a where
    toWidget :: a -> WidgetT site m ()

instance (Monad m, render ~ RY site) => ToWidget site m (render -> Html) where
    toWidget x = tell $ GWData (Body x) mempty mempty mempty mempty mempty mempty
instance (Monad m, render ~ RY site) => ToWidget site m (render -> Css) where
    toWidget x = toWidget $ CssBuilder . fromLazyText . renderCss . x
instance (Monad m, render ~ RY site) => ToWidget site m (render -> CssBuilder) where
    toWidget x = tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing $ unCssBuilder . x) mempty mempty
instance (Monad m, render ~ RY site) => ToWidget site m (render -> Javascript) where
    toWidget x = tell $ GWData mempty mempty mempty mempty mempty (Just x) mempty
instance (site' ~ site, Monad m, m' ~ m) => ToWidget site' m' (WidgetT site m ()) where
    toWidget = id
instance Monad m => ToWidget site m Html where
    toWidget = toWidget . const

-- | Allows adding some CSS to the page with a specific media type.
--
-- Since 1.2
class ToWidgetMedia site a where
    -- | Add the given content to the page, but only for the given media type.
    --
    -- Since 1.2
    toWidgetMedia :: Monad m
                  => Text -- ^ media value
                  -> a
                  -> WidgetT site m ()
instance render ~ RY site => ToWidgetMedia site (render -> Css) where
    toWidgetMedia media x = toWidgetMedia media $ CssBuilder . fromLazyText . renderCss . x
instance render ~ RY site => ToWidgetMedia site (render -> CssBuilder) where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . x) mempty mempty

class ToWidgetBody site a where
    toWidgetBody :: Monad m => a -> WidgetT site m ()

instance render ~ RY site => ToWidgetBody site (render -> Html) where
    toWidgetBody = toWidget
instance render ~ RY site => ToWidgetBody site (render -> Javascript) where
    toWidgetBody j = toWidget $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetBody site Html where
    toWidgetBody = toWidget

class ToWidgetHead site a where
    toWidgetHead :: Monad m => a -> WidgetT site m ()

instance render ~ RY site => ToWidgetHead site (render -> Html) where
    toWidgetHead = tell . GWData mempty mempty mempty mempty mempty mempty . Head
instance render ~ RY site => ToWidgetHead site (render -> Css) where
    toWidgetHead = toWidget
instance render ~ RY site => ToWidgetHead site (render -> CssBuilder) where
    toWidgetHead = toWidget
instance render ~ RY site => ToWidgetHead site (render -> Javascript) where
    toWidgetHead j = toWidgetHead $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetHead site Html where
    toWidgetHead = toWidgetHead . const

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitle :: Monad m => Html -> WidgetT site m ()
setTitle x = tell $ GWData mempty (Last $ Just $ Title x) mempty mempty mempty mempty mempty

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitleI :: (Monad m, RenderMessage site msg) => msg -> WidgetT site m ()
setTitleI msg = do
    mr <- getMessageRender
    setTitle $ toHtml $ mr msg

-- | Link to the specified local stylesheet.
addStylesheet :: Monad m => Route site -> WidgetT site m ()
addStylesheet = flip addStylesheetAttrs []

-- | Link to the specified local stylesheet.
addStylesheetAttrs :: Monad m => Route site -> [(Text, Text)] -> WidgetT site m ()
addStylesheetAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Local x) y) mempty mempty mempty

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: Monad m => Text -> WidgetT site m ()
addStylesheetRemote = flip addStylesheetRemoteAttrs []

-- | Link to the specified remote stylesheet.
addStylesheetRemoteAttrs :: Monad m => Text -> [(Text, Text)] -> WidgetT site m ()
addStylesheetRemoteAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Remote x) y) mempty mempty mempty

addStylesheetEither :: Monad m => Either (Route site) Text -> WidgetT site m ()
addStylesheetEither = either addStylesheet addStylesheetRemote

addScriptEither :: Monad m => Either (Route site) Text -> WidgetT site m ()
addScriptEither = either addScript addScriptRemote

-- | Link to the specified local script.
addScript :: Monad m => Route site -> WidgetT site m ()
addScript = flip addScriptAttrs []

-- | Link to the specified local script.
addScriptAttrs :: Monad m => Route site -> [(Text, Text)] -> WidgetT site m ()
addScriptAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Local x) y) mempty mempty mempty mempty

-- | Link to the specified remote script.
addScriptRemote :: Monad m => Text -> WidgetT site m ()
addScriptRemote = flip addScriptRemoteAttrs []

-- | Link to the specified remote script.
addScriptRemoteAttrs :: Monad m => Text -> [(Text, Text)] -> WidgetT site m ()
addScriptRemoteAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Remote x) y) mempty mempty mempty mempty

whamlet :: QuasiQuoter
whamlet = NP.hamletWithSettings rules NP.defaultHamletSettings

whamletFile :: FilePath -> Q Exp
whamletFile = NP.hamletFileWithSettings rules NP.defaultHamletSettings

whamletFileWithSettings :: NP.HamletSettings -> FilePath -> Q Exp
whamletFileWithSettings = NP.hamletFileWithSettings rules

rules :: Q NP.HamletRules
rules = do
    ah <- [|toWidget|]
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
ihamletToRepHtml :: (HandlerReader m, RenderMessage (HandlerSite m) message)
                 => HtmlUrlI18n message (Route (HandlerSite m))
                 -> m Html
ihamletToRepHtml ih = do
    urender <- getUrlRenderParams
    mrender <- getMessageRender
    return $ ih (toHtml . mrender) urender

tell :: Monad m => GWData (Route site) -> WidgetT site m ()
tell w = WidgetT $ const $ return ((), w)

toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)

liftHandlerT :: MonadIO m
             => HandlerT site IO a
             -> HandlerT site m a
liftHandlerT (HandlerT f) =
    HandlerT $ liftIO . f . fixToParent
  where
    fixToParent hd = hd { handlerToParent = const () }

liftWidget :: MonadIO m
           => WidgetT child IO a
           -> HandlerT child (HandlerT parent m) (WidgetT parent m a)
liftWidget (WidgetT f) = HandlerT $ \hd -> do
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
