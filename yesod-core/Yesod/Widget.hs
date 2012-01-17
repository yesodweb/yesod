{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
-- FIXME Should we remove the older names here (addJulius, etc)?

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
      -- * Creating
      -- ** Head of page
    , setTitle
    , setTitleI
    , addHamletHead
    , addHtmlHead
      -- ** Body
    , addHamlet
    , addHtml
    , addWidget
    , addSubWidget
      -- ** CSS
    , addCassius
    , addCassiusMedia
    , addLucius
    , addLuciusMedia
    , addStylesheet
    , addStylesheetAttrs
    , addStylesheetRemote
    , addStylesheetRemoteAttrs
    , addStylesheetEither
      -- ** Javascript
    , addJulius
    , addJuliusBody
    , addCoffee
    , addCoffeeBody
    , addScript
    , addScriptAttrs
    , addScriptRemote
    , addScriptRemoteAttrs
    , addScriptEither
      -- * Internal
    , unGWidget
    ) where

import Data.Monoid
import qualified Text.Blaze.Html5 as H
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Text.Coffee
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
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE), Pat (VarP), newName)

import Control.Monad.Trans.Control (MonadBaseControl (..), control)
import Control.Monad.Trans.Resource
import Control.Exception (throwIO)
import qualified Text.Hamlet as NP
import Data.Text.Lazy.Builder (fromLazyText)
import Text.Blaze (toHtml, preEscapedLazyText)
import Control.Monad.Base (MonadBase (liftBase))

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

-- FIXME At some point in the future, deprecate all the
-- addHamlet/Cassius/Lucius/Julius stuff. For the most part, toWidget* will be
-- sufficient. For somethings, like addLuciusMedia, create addCssUrlMedia.

type RY master = Route master -> [(Text, Text)] -> Text

instance render ~ RY master => ToWidget sub master (render -> Html) where
    toWidget = addHamlet
instance render ~ RY master => ToWidget sub master (render -> Css) where
    toWidget = addCassius
instance render ~ RY master => ToWidget sub master (render -> Javascript) where
    toWidget = addJulius
instance ToWidget sub master (GWidget sub master ()) where
    toWidget = id
instance ToWidget sub master Html where
    toWidget = addHtml
instance render ~ RY master => ToWidget sub master (render -> Coffeescript) where
    toWidget = addCoffee

class ToWidgetBody sub master a where
    toWidgetBody :: a -> GWidget sub master ()

instance render ~ RY master => ToWidgetBody sub master (render -> Html) where
    toWidgetBody = addHamlet
instance render ~ RY master => ToWidgetBody sub master (render -> Javascript) where
    toWidgetBody = addJuliusBody
instance ToWidgetBody sub master Html where
    toWidgetBody = addHtml
instance render ~ RY master => ToWidgetBody sub master (render -> Coffeescript) where
    toWidgetBody = addCoffeeBody

class ToWidgetHead sub master a where
    toWidgetHead :: a -> GWidget sub master ()

instance render ~ RY master => ToWidgetHead sub master (render -> Html) where
    toWidgetHead = addHamletHead
instance render ~ RY master => ToWidgetHead sub master (render -> Css) where
    toWidgetHead = addCassius
instance render ~ RY master => ToWidgetHead sub master (render -> Javascript) where
    toWidgetHead = addJulius
instance ToWidgetHead sub master Html where
    toWidgetHead = addHtmlHead
instance render ~ RY master => ToWidgetHead sub master (render -> Coffeescript) where
    toWidgetHead = addCoffee

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

-- | Add a 'Hamlet' to the head tag.
addHamletHead :: HtmlUrl (Route master) -> GWidget sub master ()
addHamletHead = tell . GWData mempty mempty mempty mempty mempty mempty . Head

-- | Add a 'Html' to the head tag.
addHtmlHead :: Html -> GWidget sub master ()
addHtmlHead = addHamletHead . const

-- | Add a 'Hamlet' to the body tag.
addHamlet :: HtmlUrl (Route master) -> GWidget sub master ()
addHamlet x = tell $ GWData (Body x) mempty mempty mempty mempty mempty mempty

-- | Add a 'Html' to the body tag.
addHtml :: Html -> GWidget sub master ()
addHtml = addHamlet . const

-- | Add another widget. This is defined as 'id', by can help with types, and
-- makes widget blocks look more consistent.
addWidget :: GWidget sub master () -> GWidget sub master ()
addWidget = id

-- | Add some raw CSS to the style tag. Applies to all media types.
addCassius :: CssUrl (Route master) -> GWidget sub master ()
addCassius x = tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing $ \r -> fromLazyText $ renderCss $ x r) mempty mempty

-- | Identical to 'addCassius'.
addLucius :: CssUrl (Route master) -> GWidget sub master ()
addLucius = addCassius

-- | Add some raw CSS to the style tag, for a specific media type.
addCassiusMedia :: Text -> CssUrl (Route master) -> GWidget sub master ()
addCassiusMedia m x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just m) $ \r -> fromLazyText $ renderCss $ x r) mempty mempty

-- | Identical to 'addCassiusMedia'.
addLuciusMedia :: Text -> CssUrl (Route master) -> GWidget sub master ()
addLuciusMedia = addCassiusMedia

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

-- | Include raw Javascript in the page's script tag.
addJulius :: JavascriptUrl (Route master) -> GWidget sub master ()
addJulius x = tell $ GWData mempty mempty mempty mempty mempty (Just x) mempty

-- | Add a new script tag to the body with the contents of this 'Julius'
-- template.
addJuliusBody :: JavascriptUrl (Route master) -> GWidget sub master ()
addJuliusBody j = addHamlet $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j

-- | Add Coffesscript to the page's script tag. Requires the coffeescript
-- executable to be present at runtime.
addCoffee :: CoffeeUrl (Route master) -> GWidget sub master ()
addCoffee c = do
    render <- lift getUrlRenderParams
    t <- liftIO $ renderCoffee render c
    addJulius $ const $ Javascript $ fromLazyText t

-- | Add a new script tag to the body with the contents of this Coffesscript
-- template. Requires the coffeescript executable to be present at runtime.
addCoffeeBody :: CoffeeUrl (Route master) -> GWidget sub master ()
addCoffeeBody c = do
    render <- lift getUrlRenderParams
    t <- liftIO $ renderCoffee render c
    addJuliusBody $ const $ Javascript $ fromLazyText t

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

rules :: Q NP.HamletRules
rules = do
    ah <- [|addHtml|]
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
    return $ NP.HamletRules ah ur $ \_ b -> return b

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
    fmap f (GWidget x) = GWidget (fmap (\(a, w) -> (f a, w)) x)
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

instance Resource (GWidget sub master) where
    type Base (GWidget sub master) = IO
    resourceLiftBase = liftIO
    resourceBracket_ a b c = control $ \run -> resourceBracket_ a b (run c)
instance ResourceUnsafeIO (GWidget sub master) where
    unsafeFromIO = liftIO
instance ResourceThrow (GWidget sub master) where
    resourceThrow = liftIO . throwIO
instance ResourceIO (GWidget sub master)
