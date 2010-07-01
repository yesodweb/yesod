{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
module Yesod.Widget
    ( -- * Datatype
      Widget
      -- * Unwrapping
    , widgetToPageContent
    , applyLayoutW
      -- * Creating
    , newIdent
    , setTitle
    , addStyle
    , addStylesheet
    , addStylesheetRemote
    , addScript
    , addScriptRemote
    , addHead
    , addBody
      -- * Manipulating
    , wrapWidget
    , extractBody
    ) where

import Data.List (nub)
import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Yesod.Hamlet (Hamlet, hamlet, PageContent (..), Html, string)
import Yesod.Handler (Routes, GHandler)
import Yesod.Yesod (Yesod, defaultLayout)
import Yesod.Content (RepHtml (..))
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO)

data Location url = Local url | Remote String
    deriving (Show, Eq)
locationToHamlet :: Location url -> Hamlet url
locationToHamlet (Local url) = [$hamlet|@url@|]
locationToHamlet (Remote s) = [$hamlet|$string.s$|]

newtype UniqueList x = UniqueList ([x] -> [x])
instance Monoid (UniqueList x) where
    mempty = UniqueList id
    UniqueList x `mappend` UniqueList y = UniqueList $ x . y
runUniqueList :: Eq x => UniqueList x -> [x]
runUniqueList (UniqueList x) = nub $ x []
toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)

newtype Script url = Script { unScript :: Location url }
    deriving (Show, Eq)
newtype Stylesheet url = Stylesheet { unStylesheet :: Location url }
    deriving (Show, Eq)
newtype Title = Title { unTitle :: Html () }
newtype Style url = Style (Hamlet url)
    deriving Monoid
newtype Head url = Head (Hamlet url)
    deriving Monoid
newtype Body url = Body (Hamlet url)
    deriving Monoid

newtype Widget sub master a = Widget (
    WriterT (Body (Routes master)) (
    WriterT (Last Title) (
    WriterT (UniqueList (Script (Routes master))) (
    WriterT (UniqueList (Stylesheet (Routes master))) (
    WriterT (Style (Routes master)) (
    WriterT (Head (Routes master)) (
    StateT Int (
    GHandler sub master
    ))))))) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadCatchIO)

setTitle :: Html () -> Widget sub master ()
setTitle = Widget . lift . tell . Last . Just . Title

addHead :: Hamlet (Routes master) -> Widget sub master ()
addHead = Widget . lift . lift . lift . lift . lift . tell . Head

addBody :: Hamlet (Routes master) -> Widget sub master ()
addBody = Widget . tell . Body

newIdent :: Widget sub master String
newIdent = Widget $ lift $ lift $ lift $ lift $ lift $ lift $ do
    i <- get
    let i' = i + 1
    put i'
    return $ "w" ++ show i'

addStyle :: Hamlet (Routes master) -> Widget sub master ()
addStyle = Widget . lift . lift . lift . lift . tell . Style

addStylesheet :: Routes master -> Widget sub master ()
addStylesheet = Widget . lift . lift . lift . tell . toUnique . Stylesheet . Local

addStylesheetRemote :: String -> Widget sub master ()
addStylesheetRemote =
    Widget . lift . lift . lift . tell . toUnique . Stylesheet . Remote

addScript :: Routes master -> Widget sub master ()
addScript = Widget . lift . lift . tell . toUnique . Script . Local

addScriptRemote :: String -> Widget sub master ()
addScriptRemote =
    Widget . lift . lift . tell . toUnique . Script . Remote

applyLayoutW :: (Eq (Routes m), Yesod m)
             => Widget sub m () -> GHandler sub m RepHtml
applyLayoutW w = widgetToPageContent w >>= fmap RepHtml . defaultLayout

widgetToPageContent :: Eq (Routes master)
                    => Widget sub master ()
                    -> GHandler sub master (PageContent (Routes master))
widgetToPageContent (Widget w) = do
    w' <- flip evalStateT 0
        $ runWriterT $ runWriterT $ runWriterT $ runWriterT
        $ runWriterT $ runWriterT w
    let (((((((),
         Body body),
         Last mTitle),
         scripts'),
         stylesheets'),
         Style style),
         Head head') = w'
    let title = maybe mempty unTitle mTitle
    let scripts = map (locationToHamlet . unScript) $ runUniqueList scripts'
    let stylesheets = map (locationToHamlet . unStylesheet)
                    $ runUniqueList stylesheets'
    let head'' = [$hamlet|
$forall scripts s
    %script!src=^s^
$forall stylesheets s
    %link!rel=stylesheet!href=^s^
%style
    ^style^
^head'^
|]
    return $ PageContent title head'' body

wrapWidget :: (Hamlet (Routes m) -> Hamlet (Routes m))
           -> Widget s m a -> Widget s m a
wrapWidget wrap (Widget w) =
    Widget $ mapWriterT (fmap go) w
  where
    go (a, Body h) = (a, Body $ wrap h)

extractBody :: Widget s m () -> Widget s m (Hamlet (Routes m))
extractBody (Widget w) =
    Widget $ mapWriterT (fmap go) w
  where
    go ((), Body h) = (h, Body mempty)
