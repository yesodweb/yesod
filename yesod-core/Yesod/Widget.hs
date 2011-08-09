{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import Text.Lucius (Lucius)
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

import Control.Monad.IO.Control (MonadControlIO)
import qualified Text.Hamlet as NP
import Data.Text.Lazy.Builder (fromLazyText)
import Text.Blaze (toHtml, preEscapedLazyText)

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. This is basically a large 'WriterT' stack keeping track of
-- dependencies along with a 'StateT' to track unique identifiers.
newtype GGWidget m monad a = GWidget { unGWidget :: GWInner m monad a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadControlIO)

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

instance url ~ Route master => ToWidget sub master (Hamlet url) where
    toWidget = addHamlet
instance url ~ Route master => ToWidget sub master (Cassius url) where
    toWidget = addCassius
instance url ~ Route master => ToWidget sub master (Julius url) where
    toWidget = addJulius
instance ToWidget sub master (GWidget sub master ()) where
    toWidget = id
instance ToWidget sub master Html where
    toWidget = addHtml
instance url ~ Route master => ToWidget sub master (Coffee url) where
    toWidget = addCoffee

class ToWidgetBody sub master a where
    toWidgetBody :: a -> GWidget sub master ()

instance url ~ Route master => ToWidgetBody sub master (Hamlet url) where
    toWidgetBody = addHamlet
instance url ~ Route master => ToWidgetBody sub master (Julius url) where
    toWidgetBody = addJulius
instance ToWidgetBody sub master Html where
    toWidgetBody = addHtml
instance url ~ Route master => ToWidgetBody sub master (Coffee url) where
    toWidgetBody = addCoffeeBody

class ToWidgetHead sub master a where
    toWidgetHead :: a -> GWidget sub master ()

instance url ~ Route master => ToWidgetHead sub master (Hamlet url) where
    toWidgetHead = addHamletHead
instance url ~ Route master => ToWidgetHead sub master (Cassius url) where
    toWidgetHead = addCassius
instance url ~ Route master => ToWidgetHead sub master (Julius url) where
    toWidgetHead = addJulius
instance ToWidgetHead sub master Html where
    toWidgetHead = addHtmlHead
instance url ~ Route master => ToWidgetHead sub master (Coffee url) where
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
addHamletHead :: Monad m => Hamlet (Route master) -> GGWidget master m ()
addHamletHead = GWidget . tell . GWData mempty mempty mempty mempty mempty mempty . Head

-- | Add a 'Html' to the head tag.
addHtmlHead :: Monad m => Html -> GGWidget master m ()
addHtmlHead = addHamletHead . const

-- | Add a 'Hamlet' to the body tag.
addHamlet :: Monad m => Hamlet (Route master) -> GGWidget master m ()
addHamlet x = GWidget $ tell $ GWData (Body x) mempty mempty mempty mempty mempty mempty

-- | Add a 'Html' to the body tag.
addHtml :: Monad m => Html -> GGWidget master m ()
addHtml = addHamlet . const

-- | Add another widget. This is defined as 'id', by can help with types, and
-- makes widget blocks look more consistent.
addWidget :: Monad mo => GGWidget m mo () -> GGWidget m mo ()
addWidget = id

-- | Add some raw CSS to the style tag. Applies to all media types.
addCassius :: Monad m => Cassius (Route master) -> GGWidget master m ()
addCassius x = GWidget $ tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing x) mempty mempty

-- | Identical to 'addCassius'.
addLucius :: Monad m => Lucius (Route master) -> GGWidget master m ()
addLucius = addCassius

-- | Add some raw CSS to the style tag, for a specific media type.
addCassiusMedia :: Monad m => Text -> Cassius (Route master) -> GGWidget master m ()
addCassiusMedia m x = GWidget $ tell $ GWData mempty mempty mempty mempty (Map.singleton (Just m) x) mempty mempty

-- | Identical to 'addCassiusMedia'.
addLuciusMedia :: Monad m => Text -> Lucius (Route master) -> GGWidget master m ()
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
addJulius :: Monad m => Julius (Route master) -> GGWidget master m ()
addJulius x = GWidget $ tell $ GWData mempty mempty mempty mempty mempty (Just x) mempty

-- | Add a new script tag to the body with the contents of this 'Julius'
-- template.
addJuliusBody :: Monad m => Julius (Route master) -> GGWidget master m ()
addJuliusBody j = addHamlet $ \r -> H.script $ preEscapedLazyText $ renderJulius r j

-- | Add Coffesscript to the page's script tag. Requires the coffeescript
-- executable to be present at runtime.
addCoffee :: MonadIO m => Coffee (Route master) -> GGWidget master (GGHandler sub master m) ()
addCoffee c = do
    render <- lift getUrlRenderParams
    t <- liftIO $ renderCoffee render c
    addJulius $ const $ Javascript $ fromLazyText t

-- | Add a new script tag to the body with the contents of this Coffesscript
-- template. Requires the coffeescript executable to be present at runtime.
addCoffeeBody :: MonadIO m => Coffee (Route master) -> GGWidget master (GGHandler sub master m) ()
addCoffeeBody c = do
    render <- lift getUrlRenderParams
    t <- liftIO $ renderCoffee render c
    addJuliusBody $ const $ Javascript $ fromLazyText t

-- | Pull out the HTML tag contents and return it. Useful for performing some
-- manipulations. It can be easier to use this sometimes than 'wrapWidget'.
extractBody :: Monad mo => GGWidget m mo () -> GGWidget m mo (Hamlet (Route m))
extractBody (GWidget w) =
    GWidget $ mapRWST (liftM go) w
  where
    go ((), s, GWData (Body h) b c d e f g) = (h, s, GWData (Body mempty) b c d e f g)

-- | Content for a web page. By providing this datatype, we can easily create
-- generic site templates, which would have the type signature:
--
-- > PageContent url -> Hamlet url
data PageContent url = PageContent
    { pageTitle :: Html
    , pageHead :: Hamlet url
    , pageBody :: Hamlet url
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
                 => NP.IHamlet message (Route master)
                 -> GGHandler sub master mo RepHtml
ihamletToRepHtml ih = do
    urender <- getUrlRenderParams
    mrender <- getMessageRender
    return $ RepHtml $ toContent $ ih (toHtml . mrender) urender
