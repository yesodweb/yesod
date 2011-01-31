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
    , addStylesheetRemote
    , addStylesheetEither
      -- ** Javascript
    , addJulius
    , addScript
    , addScriptRemote
    , addScriptEither
      -- * Utilities
    , extractBody
    ) where

import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
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

import Control.Monad.IO.Peel (MonadPeelIO)

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. This is basically a large 'WriterT' stack keeping track of
-- dependencies along with a 'StateT' to track unique identifiers.
newtype GGWidget s m monad a = GWidget { unGWidget :: GWInner s m monad a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPeelIO)

instance MonadTrans (GGWidget s m) where
    lift = GWidget . lift . lift . lift . lift . lift . lift . lift . lift

type GWidget s m = GGWidget s m (GHandler s m)
type GWInner sub master monad =
    WriterT (Body (Route master)) (
    WriterT (Last Title) (
    WriterT (UniqueList (Script (Route master))) (
    WriterT (UniqueList (Stylesheet (Route master))) (
    WriterT (Maybe (Cassius (Route master))) (
    WriterT (Maybe (Julius (Route master))) (
    WriterT (Head (Route master)) (
    StateT Int (
    monad
    ))))))))
instance (Monad monad, a ~ ()) => Monoid (GGWidget sub master monad a) where
    mempty = return ()
    mappend x y = x >> y

instance (Monad monad, a ~ ()) => HamletValue (GGWidget s m monad a) where
    newtype HamletMonad (GGWidget s m monad a) b =
        GWidget' { runGWidget' :: GGWidget s m monad b }
    type HamletUrl (GGWidget s m monad a) = Route m
    toHamletValue = runGWidget'
    htmlToHamletMonad = GWidget' . addHtml
    urlToHamletMonad url params = GWidget' $
        addHamlet $ \r -> preEscapedString (r url params)
    fromHamletValue = GWidget'
instance (Monad monad, a ~ ()) => Monad (HamletMonad (GGWidget s m monad a)) where
    return = GWidget' . return
    x >>= y = GWidget' $ runGWidget' x >>= runGWidget' . y

addSubWidget :: (YesodSubRoute sub master) => sub -> GWidget sub master a -> GWidget sub' master a
addSubWidget sub w = do master <- lift getYesod
                        let sr = fromSubRoute sub master
                        i <- GWidget $ lift $ lift $ lift $ lift $ lift $ lift $ lift get
                        w' <- lift $ toMasterHandlerMaybe sr (const sub) Nothing $ flip runStateT i
                              $ runWriterT $ runWriterT $ runWriterT $ runWriterT
                              $ runWriterT $ runWriterT $ runWriterT 
                              $ unGWidget w
                        let ((((((((a,
                                    body),
                                   title),
                                  scripts),
                                 stylesheets),
                                style),
                               jscript),
                              h),
                             i') = w'
                        GWidget $ do
                          tell body
                          lift $ tell title
                          lift $ lift $ tell scripts
                          lift $ lift $ lift $ tell stylesheets
                          lift $ lift $ lift $ lift $ tell style
                          lift $ lift $ lift $ lift $ lift $ tell jscript
                          lift $ lift $ lift $ lift $ lift $ lift $ tell h
                          lift $ lift $ lift $ lift $ lift $ lift $ lift $ put i'
                          return a

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitle :: Monad m => Html -> GGWidget sub master m ()
setTitle = GWidget . lift . tell . Last . Just . Title

-- | Add a 'Hamlet' to the head tag.
addHamletHead :: Monad m => Hamlet (Route master) -> GGWidget sub master m ()
addHamletHead = GWidget . lift . lift . lift . lift . lift . lift . tell . Head

-- | Add a 'Html' to the head tag.
addHtmlHead :: Monad m => Html -> GGWidget sub master m ()
addHtmlHead = GWidget . lift . lift . lift . lift . lift . lift . tell . Head . const

-- | Add a 'Hamlet' to the body tag.
addHamlet :: Monad m => Hamlet (Route master) -> GGWidget sub master m ()
addHamlet = GWidget . tell . Body

-- | Add a 'Html' to the body tag.
addHtml :: Monad m => Html -> GGWidget sub master m ()
addHtml = GWidget . tell . Body . const

-- | Add another widget. This is defined as 'id', by can help with types, and
-- makes widget blocks look more consistent.
addWidget :: Monad mo => GGWidget s m mo () -> GGWidget s m mo ()
addWidget = id

-- | Add some raw CSS to the style tag.
addCassius :: Monad m => Cassius (Route master) -> GGWidget sub master m ()
addCassius = GWidget . lift . lift . lift . lift . tell . Just

-- | Link to the specified local stylesheet.
addStylesheet :: Monad m => Route master -> GGWidget sub master m ()
addStylesheet = GWidget . lift . lift . lift . tell . toUnique . Stylesheet . Local

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: Monad m => String -> GGWidget sub master m ()
addStylesheetRemote =
    GWidget . lift . lift . lift . tell . toUnique . Stylesheet . Remote

addStylesheetEither :: Monad m => Either (Route master) String -> GGWidget sub master m ()
addStylesheetEither = either addStylesheet addStylesheetRemote

addScriptEither :: Monad m => Either (Route master) String -> GGWidget sub master m ()
addScriptEither = either addScript addScriptRemote

-- | Link to the specified local script.
addScript :: Monad m => Route master -> GGWidget sub master m ()
addScript = GWidget . lift . lift . tell . toUnique . Script . Local

-- | Link to the specified remote script.
addScriptRemote :: Monad m => String -> GGWidget sub master m ()
addScriptRemote =
    GWidget . lift . lift . tell . toUnique . Script . Remote

-- | Include raw Javascript in the page's script tag.
addJulius :: Monad m => Julius (Route master) -> GGWidget sub master m ()
addJulius = GWidget . lift . lift . lift . lift . lift. tell . Just

-- | Pull out the HTML tag contents and return it. Useful for performing some
-- manipulations. It can be easier to use this sometimes than 'wrapWidget'.
extractBody :: Monad mo => GGWidget s m mo () -> GGWidget s m mo (Hamlet (Route m))
extractBody (GWidget w) =
    GWidget $ mapWriterT (liftM go) w
  where
    go ((), Body h) = (h, Body mempty)

-- | Content for a web page. By providing this datatype, we can easily create
-- generic site templates, which would have the type signature:
--
-- > PageContent url -> Hamlet url
data PageContent url = PageContent
    { pageTitle :: Html
    , pageHead :: Hamlet url
    , pageBody :: Hamlet url
    }
