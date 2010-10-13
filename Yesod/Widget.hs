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
    , newIdent
    , setTitle
    , addStyle
    , addStylesheet
    , addStylesheetRemote
    , addStylesheetEither
    , addScript
    , addScriptRemote
    , addScriptEither
    , addHead
    , addBody
    , addJavascript
      -- * Manipulating
    , wrapWidget
    , extractBody
    ) where

import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Yesod.Handler (Route, GHandler)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO)
import Yesod.Internal

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. This is basically a large 'WriterT' stack keeping track of
-- dependencies along with a 'StateT' to track unique identifiers.
newtype GWidget sub master a = GWidget (
    WriterT (Body (Route master)) (
    WriterT (Last Title) (
    WriterT (UniqueList (Script (Route master))) (
    WriterT (UniqueList (Stylesheet (Route master))) (
    WriterT (Maybe (Cassius (Route master))) (
    WriterT (Maybe (Julius (Route master))) (
    WriterT (Head (Route master)) (
    StateT Int (
    GHandler sub master
    )))))))) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadCatchIO)
instance Monoid (GWidget sub master ()) where
    mempty = return ()
    mappend x y = x >> y

instance HamletValue (GWidget s m ()) where
    newtype HamletMonad (GWidget s m ()) a =
        GWidget' { runGWidget' :: GWidget s m a }
    type HamletUrl (GWidget s m ()) = Route m
    toHamletValue = runGWidget'
    htmlToHamletMonad = GWidget' . addBody . const
    urlToHamletMonad url params = GWidget' $
        addBody $ \r -> preEscapedString (r url params)
    fromHamletValue = GWidget'
instance Monad (HamletMonad (GWidget s m ())) where
    return = GWidget' . return
    x >>= y = GWidget' $ runGWidget' x >>= runGWidget' . y

-- | Lift an action in the 'GHandler' monad into an action in the 'GWidget'
-- monad.
liftHandler :: GHandler sub master a -> GWidget sub master a
liftHandler = GWidget . lift . lift . lift . lift . lift . lift . lift . lift

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitle :: Html -> GWidget sub master ()
setTitle = GWidget . lift . tell . Last . Just . Title

-- | Add some raw HTML to the head tag.
addHead :: Hamlet (Route master) -> GWidget sub master ()
addHead = GWidget . lift . lift . lift . lift . lift . lift . tell . Head

-- | Add some raw HTML to the body tag.
addBody :: Hamlet (Route master) -> GWidget sub master ()
addBody = GWidget . tell . Body

-- | Get a unique identifier.
newIdent :: GWidget sub master String
newIdent = GWidget $ lift $ lift $ lift $ lift $ lift $ lift $ lift $ do
    i <- get
    let i' = i + 1
    put i'
    return $ "w" ++ show i'

-- | Add some raw CSS to the style tag.
addStyle :: Cassius (Route master) -> GWidget sub master ()
addStyle = GWidget . lift . lift . lift . lift . tell . Just

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
addJavascript :: Julius (Route master) -> GWidget sub master ()
addJavascript = GWidget . lift . lift . lift . lift . lift. tell . Just

-- | Modify the given 'GWidget' by wrapping the body tag HTML code with the
-- given function. You might also consider using 'extractBody'.
wrapWidget :: GWidget s m a
           -> (Hamlet (Route m) -> Hamlet (Route m))
           -> GWidget s m a
wrapWidget (GWidget w) wrap =
    GWidget $ mapWriterT (fmap go) w
  where
    go (a, Body h) = (a, Body $ wrap h)

-- | Pull out the HTML tag contents and return it. Useful for performing some
-- manipulations. It can be easier to use this sometimes than 'wrapWidget'.
extractBody :: GWidget s m () -> GWidget s m (Hamlet (Route m))
extractBody (GWidget w) =
    GWidget $ mapWriterT (fmap go) w
  where
    go ((), Body h) = (h, Body mempty)
