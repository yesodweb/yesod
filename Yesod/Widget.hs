{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Widgets combine HTML with JS and CSS dependencies with a unique identifier
-- generator, allowing you to create truly modular HTML components.
module Yesod.Widget
    ( -- * Datatype
      GWidget (..)
    , liftHandler
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
    , newIdent
    ) where

import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Yesod.Handler (Route, GHandler, HandlerData, YesodSubRoute(..), toMasterHandlerMaybe, getYesod)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Yesod.Internal

import Control.Monad.Invert (MonadInvertIO (..))
import Control.Monad (liftM)
import qualified Data.Map as Map

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. This is basically a large 'WriterT' stack keeping track of
-- dependencies along with a 'StateT' to track unique identifiers.
newtype GWidget s m a = GWidget { unGWidget :: GWInner s m a }
    deriving (Functor, Applicative, Monad, MonadIO)
type GWInner sub master =
    WriterT (Body (Route master)) (
    WriterT (Last Title) (
    WriterT (UniqueList (Script (Route master))) (
    WriterT (UniqueList (Stylesheet (Route master))) (
    WriterT (Maybe (Cassius (Route master))) (
    WriterT (Maybe (Julius (Route master))) (
    WriterT (Head (Route master)) (
    StateT Int (
    GHandler sub master
    ))))))))
instance Monoid (GWidget sub master ()) where
    mempty = return ()
    mappend x y = x >> y
instance MonadInvertIO (GWidget s m) where
    newtype InvertedIO (GWidget s m) a =
        InvGWidgetIO
            { runInvGWidgetIO :: InvertedIO (GWInner s m) a
            }
    type InvertedArg (GWidget s m) =
        (Int, (HandlerData s m, (Map.Map String String, ())))
    invertIO = liftM (fmap InvGWidgetIO) . invertIO . unGWidget
    revertIO f = GWidget $ revertIO $ liftM runInvGWidgetIO . f

instance HamletValue (GWidget s m ()) where
    newtype HamletMonad (GWidget s m ()) a =
        GWidget' { runGWidget' :: GWidget s m a }
    type HamletUrl (GWidget s m ()) = Route m
    toHamletValue = runGWidget'
    htmlToHamletMonad = GWidget' . addHtml
    urlToHamletMonad url params = GWidget' $
        addHamlet $ \r -> preEscapedString (r url params)
    fromHamletValue = GWidget'
instance Monad (HamletMonad (GWidget s m ())) where
    return = GWidget' . return
    x >>= y = GWidget' $ runGWidget' x >>= runGWidget' . y

-- | Lift an action in the 'GHandler' monad into an action in the 'GWidget'
-- monad.
liftHandler :: GHandler sub master a -> GWidget sub master a
liftHandler = GWidget . lift . lift . lift . lift . lift . lift . lift . lift

addSubWidget :: (YesodSubRoute sub master) => sub -> GWidget sub master a -> GWidget sub' master a
addSubWidget sub w = do master <- liftHandler getYesod
                        let sr = fromSubRoute sub master
                        i <- GWidget $ lift $ lift $ lift $ lift $ lift $ lift $ lift get
                        w' <- liftHandler $ toMasterHandlerMaybe sr (const sub) Nothing $ flip runStateT i
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
setTitle :: Html -> GWidget sub master ()
setTitle = GWidget . lift . tell . Last . Just . Title

-- | Add a 'Hamlet' to the head tag.
addHamletHead :: Hamlet (Route master) -> GWidget sub master ()
addHamletHead = GWidget . lift . lift . lift . lift . lift . lift . tell . Head

-- | Add a 'Html' to the head tag.
addHtmlHead :: Html -> GWidget sub master ()
addHtmlHead = GWidget . lift . lift . lift . lift . lift . lift . tell . Head . const

-- | Add a 'Hamlet' to the body tag.
addHamlet :: Hamlet (Route master) -> GWidget sub master ()
addHamlet = GWidget . tell . Body

-- | Add a 'Html' to the body tag.
addHtml :: Html -> GWidget sub master ()
addHtml = GWidget . tell . Body . const

-- | Add another widget. This is defined as 'id', by can help with types, and
-- makes widget blocks look more consistent.
addWidget :: GWidget s m () -> GWidget s m ()
addWidget = id

-- | Get a unique identifier.
newIdent :: GWidget sub master String
newIdent = GWidget $ lift $ lift $ lift $ lift $ lift $ lift $ lift $ do
    i <- get
    let i' = i + 1
    put i'
    return $ "w" ++ show i'

-- | Add some raw CSS to the style tag.
addCassius :: Cassius (Route master) -> GWidget sub master ()
addCassius = GWidget . lift . lift . lift . lift . tell . Just

-- | Link to the specified local stylesheet.
addStylesheet :: Route master -> GWidget sub master ()
addStylesheet = GWidget . lift . lift . lift . tell . toUnique . Stylesheet . Local

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: String -> GWidget sub master ()
addStylesheetRemote =
    GWidget . lift . lift . lift . tell . toUnique . Stylesheet . Remote

addStylesheetEither :: Either (Route master) String -> GWidget sub master ()
addStylesheetEither = either addStylesheet addStylesheetRemote

addScriptEither :: Either (Route master) String -> GWidget sub master ()
addScriptEither = either addScript addScriptRemote

-- | Link to the specified local script.
addScript :: Route master -> GWidget sub master ()
addScript = GWidget . lift . lift . tell . toUnique . Script . Local

-- | Link to the specified remote script.
addScriptRemote :: String -> GWidget sub master ()
addScriptRemote =
    GWidget . lift . lift . tell . toUnique . Script . Remote

-- | Include raw Javascript in the page's script tag.
addJulius :: Julius (Route master) -> GWidget sub master ()
addJulius = GWidget . lift . lift . lift . lift . lift. tell . Just

-- | Pull out the HTML tag contents and return it. Useful for performing some
-- manipulations. It can be easier to use this sometimes than 'wrapWidget'.
extractBody :: GWidget s m () -> GWidget s m (Hamlet (Route m))
extractBody (GWidget w) =
    GWidget $ mapWriterT (fmap go) w
  where
    go ((), Body h) = (h, Body mempty)
