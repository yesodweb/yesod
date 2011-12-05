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
    , GGWidget (..)
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
      -- * Utilities
    , extractBody
    ) where

import Data.Monoid
import Control.Monad.Trans.RWS
import qualified Text.Blaze.Html5 as H
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Text.Coffee
import Yesod.Handler
    (Route, GHandler, GGHandler, YesodSubRoute(..), toMasterHandlerMaybe, getYesod
    , getMessageRender, getUrlRenderParams
    )
import Yesod.Message (RenderMessage)
import Yesod.Content (RepHtml (..), toContent)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Yesod.Internal
import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Map as Map
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE), Pat (VarP), newName)

#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadTransControl (..), MonadBaseControl (..), defaultLiftBaseWith, defaultRestoreM, ComposeSt)
#else
import Control.Monad.IO.Control (MonadControlIO)
#endif
import qualified Text.Hamlet as NP
import Data.Text.Lazy.Builder (fromLazyText)
import Text.Blaze (toHtml, preEscapedLazyText)
import Control.Monad.Base (MonadBase (liftBase))

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. This is basically a large 'WriterT' stack keeping track of
-- dependencies along with a 'StateT' to track unique identifiers.
newtype GGWidget m monad a = GWidget { unGWidget :: GWInner m monad a }
    deriving (Functor, Applicative, Monad, MonadIO
#if !MIN_VERSION_monad_control(0, 3, 0)
    , MonadControlIO
#endif
    )

instance MonadBase b m => MonadBase b (GGWidget master m) where
    liftBase = lift . liftBase
#if MIN_VERSION_monad_control(0, 3, 0)
instance MonadTransControl (GGWidget master) where
    newtype StT (GGWidget master) a = StRWS {unStRWS :: (a, Int, GWData (Route master))}
    liftWith f = GWidget $ RWST $ \r s -> liftM (\x -> (x, s, mempty))
                                     (f $ \t -> liftM StRWS $ runRWST (unGWidget t) r s)
    restoreT mSt = GWidget $ RWST $ \_ _ -> liftM unStRWS mSt
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}
instance MonadBaseControl b m => MonadBaseControl b (GGWidget master m) where
     newtype StM (GGWidget master m) a = StMT {unStMT :: ComposeSt (GGWidget master) m a}
     liftBaseWith = defaultLiftBaseWith StMT
     restoreM     = defaultRestoreM   unStMT
#endif

instance MonadTrans (GGWidget m) where
    lift = GWidget . lift

type GWidget s m = GGWidget m (GHandler s m)
type GWInner master = RWST () (GWData (Route master)) Int

instance (Monad monad, a ~ ()) => Monoid (GGWidget master monad a) where
    mempty = return ()
    mappend x y = x >> y

addSubWidget :: (YesodSubRoute sub master) => sub -> GWidget sub master a -> GWidget sub' master a
addSubWidget sub (GWidget w) = do
    master <- lift getYesod
    let sr = fromSubRoute sub master
    s <- GWidget get
    (a, s', w') <- lift $ toMasterHandlerMaybe sr (const sub) Nothing $ runRWST w () s
    GWidget $ put s'
    GWidget $ tell w'
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
setTitle :: Monad m => Html -> GGWidget master m ()
setTitle x = GWidget $ tell $ GWData mempty (Last $ Just $ Title x) mempty mempty mempty mempty mempty

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitleI :: (RenderMessage master msg, Monad m) => msg -> GGWidget master (GGHandler sub master m) ()
setTitleI msg = do
    mr <- lift getMessageRender
    setTitle $ toHtml $ mr msg

-- | Add a 'Hamlet' to the head tag.
addHamletHead :: Monad m => HtmlUrl (Route master) -> GGWidget master m ()
addHamletHead = GWidget . tell . GWData mempty mempty mempty mempty mempty mempty . Head

-- | Add a 'Html' to the head tag.
addHtmlHead :: Monad m => Html -> GGWidget master m ()
addHtmlHead = addHamletHead . const

-- | Add a 'Hamlet' to the body tag.
addHamlet :: Monad m => HtmlUrl (Route master) -> GGWidget master m ()
addHamlet x = GWidget $ tell $ GWData (Body x) mempty mempty mempty mempty mempty mempty

-- | Add a 'Html' to the body tag.
addHtml :: Monad m => Html -> GGWidget master m ()
addHtml = addHamlet . const

-- | Add another widget. This is defined as 'id', by can help with types, and
-- makes widget blocks look more consistent.
addWidget :: Monad mo => GGWidget m mo () -> GGWidget m mo ()
addWidget = id

-- | Add some raw CSS to the style tag. Applies to all media types.
addCassius :: Monad m => CssUrl (Route master) -> GGWidget master m ()
addCassius x = GWidget $ tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing $ \r -> fromLazyText $ renderCss $ x r) mempty mempty

-- | Identical to 'addCassius'.
addLucius :: Monad m => CssUrl (Route master) -> GGWidget master m ()
addLucius = addCassius

-- | Add some raw CSS to the style tag, for a specific media type.
addCassiusMedia :: Monad m => Text -> CssUrl (Route master) -> GGWidget master m ()
addCassiusMedia m x = GWidget $ tell $ GWData mempty mempty mempty mempty (Map.singleton (Just m) $ \r -> fromLazyText $ renderCss $ x r) mempty mempty

-- | Identical to 'addCassiusMedia'.
addLuciusMedia :: Monad m => Text -> CssUrl (Route master) -> GGWidget master m ()
addLuciusMedia = addCassiusMedia

-- | Link to the specified local stylesheet.
addStylesheet :: Monad m => Route master -> GGWidget master m ()
addStylesheet = flip addStylesheetAttrs []

-- | Link to the specified local stylesheet.
addStylesheetAttrs :: Monad m => Route master -> [(Text, Text)] -> GGWidget master m ()
addStylesheetAttrs x y = GWidget $ tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Local x) y) mempty mempty mempty

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: Monad m => Text -> GGWidget master m ()
addStylesheetRemote = flip addStylesheetRemoteAttrs []

-- | Link to the specified remote stylesheet.
addStylesheetRemoteAttrs :: Monad m => Text -> [(Text, Text)] -> GGWidget master m ()
addStylesheetRemoteAttrs x y = GWidget $ tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Remote x) y) mempty mempty mempty

addStylesheetEither :: Monad m => Either (Route master) Text -> GGWidget master m ()
addStylesheetEither = either addStylesheet addStylesheetRemote

addScriptEither :: Monad m => Either (Route master) Text -> GGWidget master m ()
addScriptEither = either addScript addScriptRemote

-- | Link to the specified local script.
addScript :: Monad m => Route master -> GGWidget master m ()
addScript = flip addScriptAttrs []

-- | Link to the specified local script.
addScriptAttrs :: Monad m => Route master -> [(Text, Text)] -> GGWidget master m ()
addScriptAttrs x y = GWidget $ tell $ GWData mempty mempty (toUnique $ Script (Local x) y) mempty mempty mempty mempty

-- | Link to the specified remote script.
addScriptRemote :: Monad m => Text -> GGWidget master m ()
addScriptRemote = flip addScriptRemoteAttrs []

-- | Link to the specified remote script.
addScriptRemoteAttrs :: Monad m => Text -> [(Text, Text)] -> GGWidget master m ()
addScriptRemoteAttrs x y = GWidget $ tell $ GWData mempty mempty (toUnique $ Script (Remote x) y) mempty mempty mempty mempty

-- | Include raw Javascript in the page's script tag.
addJulius :: Monad m => JavascriptUrl (Route master) -> GGWidget master m ()
addJulius x = GWidget $ tell $ GWData mempty mempty mempty mempty mempty (Just x) mempty

-- | Add a new script tag to the body with the contents of this 'Julius'
-- template.
addJuliusBody :: Monad m => JavascriptUrl (Route master) -> GGWidget master m ()
addJuliusBody j = addHamlet $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j

-- | Add Coffesscript to the page's script tag. Requires the coffeescript
-- executable to be present at runtime.
addCoffee :: MonadIO m => CoffeeUrl (Route master) -> GGWidget master (GGHandler sub master m) ()
addCoffee c = do
    render <- lift getUrlRenderParams
    t <- liftIO $ renderCoffee render c
    addJulius $ const $ Javascript $ fromLazyText t

-- | Add a new script tag to the body with the contents of this Coffesscript
-- template. Requires the coffeescript executable to be present at runtime.
addCoffeeBody :: MonadIO m => CoffeeUrl (Route master) -> GGWidget master (GGHandler sub master m) ()
addCoffeeBody c = do
    render <- lift getUrlRenderParams
    t <- liftIO $ renderCoffee render c
    addJuliusBody $ const $ Javascript $ fromLazyText t

-- | Pull out the HTML tag contents and return it. Useful for performing some
-- manipulations. It can be easier to use this sometimes than 'wrapWidget'.
extractBody :: Monad mo => GGWidget m mo () -> GGWidget m mo (HtmlUrl (Route m))
extractBody (GWidget w) =
    GWidget $ mapRWST (liftM go) w
  where
    go ((), s, GWData (Body h) b c d e f g) = (h, s, GWData (Body mempty) b c d e f g)

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
                    (Just $ helper [|lift getUrlRenderParams|])
                    (Just $ helper [|liftM (toHtml .) $ lift getMessageRender|])
            f env
    return $ NP.HamletRules ah ur $ \_ b -> return b

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
ihamletToRepHtml :: (Monad mo, RenderMessage master message)
                 => HtmlUrlI18n message (Route master)
                 -> GGHandler sub master mo RepHtml
ihamletToRepHtml ih = do
    urender <- getUrlRenderParams
    mrender <- getMessageRender
    return $ RepHtml $ toContent $ ih (toHtml . mrender) urender
