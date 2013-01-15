{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
-- | Widgets combine HTML with JS and CSS dependencies with a unique identifier
-- generator, allowing you to create truly modular HTML components.
module Yesod.Widget
    ( -- * Datatype
      GWidget
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
      -- ** Body
    , addSubWidget
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
      -- * Internal
    , unGWidget
    , whamletFileWithSettings
    ) where

import Data.Monoid
import qualified Text.Blaze.Html5 as H
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Yesod.Routes.Class
import Yesod.Handler
    ( GHandler, YesodSubRoute(..), toMasterHandlerMaybe, getYesod
    , getMessageRender, getUrlRenderParams, MonadLift (..)
    )
import Yesod.Message (RenderMessage)
import Yesod.Content (RepHtml (..), toContent)
import Control.Applicative (Applicative (..), (<$>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Yesod.Internal
import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Map as Map
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE, AppE), Pat (VarP), newName)

import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Exception (throwIO)
import qualified Text.Hamlet as NP
import Data.Text.Lazy.Builder (fromLazyText, Builder)
import Text.Blaze.Html (toHtml, preEscapedToMarkup)
import qualified Data.Text.Lazy as TL
import Control.Monad.Base (MonadBase (liftBase))
import Control.Arrow (first)
import Control.Monad.Trans.Resource

import Control.Monad.Logger

preEscapedLazyText :: TL.Text -> Html
preEscapedLazyText = preEscapedToMarkup

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. While this is simply a @WriterT@, we define a newtype for
-- better error messages.
newtype GWidget sub master a = GWidget
    { unGWidget :: GHandler sub master (a, GWData (Route master))
    }

instance (a ~ ()) => Monoid (GWidget sub master a) where
    mempty = return ()
    mappend x y = x >> y

addSubWidget :: (YesodSubRoute sub master) => sub -> GWidget sub master a -> GWidget sub' master a
addSubWidget sub (GWidget w) = do
    master <- lift getYesod
    let sr = fromSubRoute sub master
    (a, w') <- lift $ toMasterHandlerMaybe sr (const sub) Nothing w
    tell w'
    return a

class ToWidget sub master a where
    toWidget :: a -> GWidget sub master ()

type RY master = Route master -> [(Text, Text)] -> Text

-- | Newtype wrapper allowing injection of arbitrary content into CSS.
--
-- Usage:
--
-- > toWidget $ CssBuilder "p { color: red }"
--
-- Since: 1.1.3
newtype CssBuilder = CssBuilder { unCssBuilder :: Builder }

instance render ~ RY master => ToWidget sub master (render -> Html) where
    toWidget x = tell $ GWData (Body x) mempty mempty mempty mempty mempty mempty
instance render ~ RY master => ToWidget sub master (render -> Css) where
    toWidget x = toWidget $ CssBuilder . fromLazyText . renderCss . x
instance render ~ RY master => ToWidget sub master (render -> CssBuilder) where
    toWidget x = tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing $ unCssBuilder . x) mempty mempty
instance render ~ RY master => ToWidget sub master (render -> Javascript) where
    toWidget x = tell $ GWData mempty mempty mempty mempty mempty (Just x) mempty
instance (sub' ~ sub, master' ~ master) => ToWidget sub' master' (GWidget sub master ()) where
    toWidget = id
instance ToWidget sub master Html where
    toWidget = toWidget . const

-- | Allows adding some CSS to the page with a specific media type.
--
-- Since 1.2
class ToWidgetMedia sub master a where
    -- | Add the given content to the page, but only for the given media type.
    --
    -- Since 1.2
    toWidgetMedia :: Text -- ^ media value
                  -> a
                  -> GWidget sub master ()
instance render ~ RY master => ToWidgetMedia sub master (render -> Css) where
    toWidgetMedia media x = toWidgetMedia media $ CssBuilder . fromLazyText . renderCss . x
instance render ~ RY master => ToWidgetMedia sub master (render -> CssBuilder) where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . x) mempty mempty

class ToWidgetBody sub master a where
    toWidgetBody :: a -> GWidget sub master ()

instance render ~ RY master => ToWidgetBody sub master (render -> Html) where
    toWidgetBody = toWidget
instance render ~ RY master => ToWidgetBody sub master (render -> Javascript) where
    toWidgetBody j = toWidget $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetBody sub master Html where
    toWidgetBody = toWidget

class ToWidgetHead sub master a where
    toWidgetHead :: a -> GWidget sub master ()

instance render ~ RY master => ToWidgetHead sub master (render -> Html) where
    toWidgetHead = tell . GWData mempty mempty mempty mempty mempty mempty . Head
instance render ~ RY master => ToWidgetHead sub master (render -> Css) where
    toWidgetHead = toWidget
instance render ~ RY master => ToWidgetHead sub master (render -> CssBuilder) where
    toWidgetHead = toWidget
instance render ~ RY master => ToWidgetHead sub master (render -> Javascript) where
    toWidgetHead j = toWidgetHead $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetHead sub master Html where
    toWidgetHead = toWidgetHead . const

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitle :: Html -> GWidget sub master ()
setTitle x = tell $ GWData mempty (Last $ Just $ Title x) mempty mempty mempty mempty mempty

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitleI :: RenderMessage master msg => msg -> GWidget sub master ()
setTitleI msg = do
    mr <- lift getMessageRender
    setTitle $ toHtml $ mr msg

-- | Link to the specified local stylesheet.
addStylesheet :: Route master -> GWidget sub master ()
addStylesheet = flip addStylesheetAttrs []

-- | Link to the specified local stylesheet.
addStylesheetAttrs :: Route master -> [(Text, Text)] -> GWidget sub master ()
addStylesheetAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Local x) y) mempty mempty mempty

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: Text -> GWidget sub master ()
addStylesheetRemote = flip addStylesheetRemoteAttrs []

-- | Link to the specified remote stylesheet.
addStylesheetRemoteAttrs :: Text -> [(Text, Text)] -> GWidget sub master ()
addStylesheetRemoteAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Remote x) y) mempty mempty mempty

addStylesheetEither :: Either (Route master) Text -> GWidget sub master ()
addStylesheetEither = either addStylesheet addStylesheetRemote

addScriptEither :: Either (Route master) Text -> GWidget sub master ()
addScriptEither = either addScript addScriptRemote

-- | Link to the specified local script.
addScript :: Route master -> GWidget sub master ()
addScript = flip addScriptAttrs []

-- | Link to the specified local script.
addScriptAttrs :: Route master -> [(Text, Text)] -> GWidget sub master ()
addScriptAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Local x) y) mempty mempty mempty mempty

-- | Link to the specified remote script.
addScriptRemote :: Text -> GWidget sub master ()
addScriptRemote = flip addScriptRemoteAttrs []

-- | Link to the specified remote script.
addScriptRemoteAttrs :: Text -> [(Text, Text)] -> GWidget sub master ()
addScriptRemoteAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Remote x) y) mempty mempty mempty mempty

-- | Content for a web page. By providing this datatype, we can easily create
-- generic site templates, which would have the type signature:
--
-- > PageContent url -> HtmlUrl url
data PageContent url = PageContent
    { pageTitle :: Html
    , pageHead :: HtmlUrl url
    , pageBody :: HtmlUrl url
    }

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
                    (Just $ helper [|liftW getUrlRenderParams|])
                    (Just $ helper [|liftM (toHtml .) $ liftW getMessageRender|])
            f env
    return $ NP.HamletRules ah ur $ \_ b -> return $ ah `AppE` b

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
ihamletToRepHtml :: RenderMessage master message
                 => HtmlUrlI18n message (Route master)
                 -> GHandler sub master RepHtml
ihamletToRepHtml ih = do
    urender <- getUrlRenderParams
    mrender <- getMessageRender
    return $ RepHtml $ toContent $ ih (toHtml . mrender) urender

tell :: GWData (Route master) -> GWidget sub master ()
tell w = GWidget $ return ((), w)

instance MonadLift (GHandler sub master) (GWidget sub master) where
    lift = GWidget . fmap (\x -> (x, mempty))

-- | Type-restricted version of @lift@
liftW :: GHandler sub master a -> GWidget sub master a
liftW = lift

-- Instances for GWidget
instance Functor (GWidget sub master) where
    fmap f (GWidget x) = GWidget (fmap (first f) x)
instance Applicative (GWidget sub master) where
    pure a = GWidget $ pure (a, mempty)
    GWidget f <*> GWidget v =
        GWidget $ k <$> f <*> v
      where
        k (a, wa) (b, wb) = (a b, wa `mappend` wb)
instance Monad (GWidget sub master) where
    return = pure
    GWidget x >>= f = GWidget $ do
        (a, wa) <- x
        (b, wb) <- unGWidget (f a)
        return (b, wa `mappend` wb)
instance MonadIO (GWidget sub master) where
    liftIO = GWidget . fmap (\a -> (a, mempty)) . liftIO
instance MonadBase IO (GWidget sub master) where
    liftBase = GWidget . fmap (\a -> (a, mempty)) . liftBase
instance MonadBaseControl IO (GWidget sub master) where
    data StM (GWidget sub master) a =
        StW (StM (GHandler sub master) (a, GWData (Route master)))
    liftBaseWith f = GWidget $ liftBaseWith $ \runInBase ->
        liftM (\x -> (x, mempty))
        (f $ liftM StW . runInBase . unGWidget)
    restoreM (StW base) = GWidget $ restoreM base

instance MonadUnsafeIO (GWidget sub master) where
    unsafeLiftIO = liftIO
instance MonadThrow (GWidget sub master) where
    monadThrow = liftIO . throwIO
instance MonadResource (GWidget sub master) where
#if MIN_VERSION_resourcet(0,4,0)
    liftResourceT = lift . liftResourceT
#else
    allocate a = lift . allocate a
    register = lift . register
    release = lift . release
    resourceMask = lift . resourceMask
#endif

instance MonadLogger (GWidget sub master) where
    monadLoggerLog a b = lift . monadLoggerLog a b
    monadLoggerLogSource a b c = lift . monadLoggerLogSource a b c
