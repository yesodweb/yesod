{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Widgets combine HTML with JS and CSS dependencies with a unique identifier
-- generator, allowing you to create truly modular HTML components.
module Yesod.Widget
    ( -- * Datatype
      GWidget
    , GGWidget (..)
    , PageContent (..)
      -- * Creating
      -- ** Head of page
    , setTitle
    , addHamletHead
    , addHtmlHead
      -- ** Body
    , addHamlet
    , addHtml
    , addWidget
    , addSubWidget
      -- ** CSS
    , addCassius
    , addStylesheet
    , addStylesheetAttrs
    , addStylesheetRemote
    , addStylesheetRemoteAttrs
    , addStylesheetEither
      -- ** Javascript
    , addJulius
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
import Text.Blaze (preEscapedText)
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Yesod.Handler
    (Route, GHandler, YesodSubRoute(..), toMasterHandlerMaybe, getYesod)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Yesod.Internal
import Control.Monad (liftM)
import Data.Text (Text)

import Control.Monad.IO.Control (MonadControlIO)

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

instance (Monad monad, a ~ ()) => HamletValue (GGWidget m monad a) where
    newtype HamletMonad (GGWidget m monad a) b =
        GWidget' { runGWidget' :: GGWidget m monad b }
    type HamletUrl (GGWidget m monad a) = Route m
    toHamletValue = runGWidget'
    htmlToHamletMonad = GWidget' . addHtml
    urlToHamletMonad url params = GWidget' $
        addHamlet $ \r -> preEscapedText (r url params)
    fromHamletValue = GWidget'
instance (Monad monad, a ~ ()) => Monad (HamletMonad (GGWidget m monad a)) where
    return = GWidget' . return
    x >>= y = GWidget' $ runGWidget' x >>= runGWidget' . y

addSubWidget :: (YesodSubRoute sub master) => sub -> GWidget sub master a -> GWidget sub' master a
addSubWidget sub (GWidget w) = do
    master <- lift getYesod
    let sr = fromSubRoute sub master
    s <- GWidget get
    (a, s', w') <- lift $ toMasterHandlerMaybe sr (const sub) Nothing $ runRWST w () s
    GWidget $ put s'
    GWidget $ tell w'
    return a

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitle :: Monad m => Html -> GGWidget master m ()
setTitle x = GWidget $ tell $ GWData mempty (Last $ Just $ Title x) mempty mempty mempty mempty mempty

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

-- | Add some raw CSS to the style tag.
addCassius :: Monad m => Cassius (Route master) -> GGWidget master m ()
addCassius x = GWidget $ tell $ GWData mempty mempty mempty mempty (Just x) mempty mempty

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
