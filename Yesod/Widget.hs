{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Widgets combine HTML with JS and CSS dependencies with a unique identifier
-- generator, allowing you to create truly modular HTML components.
module Yesod.Widget
    ( -- * Datatype
      GWidget
    , Widget
    , liftHandler
      -- * Unwrapping
    , widgetToPageContent
    , applyLayoutW
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
    , addJavaScript
      -- * Manipulating
    , wrapWidget
    , extractBody
    ) where

import Data.List (nub)
import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Yesod.Hamlet (Hamlet, hamlet, PageContent (..), Html)
import Text.Camlet
import Text.Jamlet
import Yesod.Handler (Route, GHandler)
import Yesod.Yesod (Yesod, defaultLayout)
import Yesod.Content (RepHtml (..))
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Text.Blaze (unsafeByteString)

data Location url = Local url | Remote String
    deriving (Show, Eq)
locationToHamlet :: Location url -> Hamlet url
locationToHamlet (Local url) = [$hamlet|@url@|]
locationToHamlet (Remote s) = [$hamlet|$s$|]

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
newtype Head url = Head (Hamlet url)
    deriving Monoid
newtype Body url = Body (Hamlet url)
    deriving Monoid

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. This is basically a large 'WriterT' stack keeping track of
-- dependencies along with a 'StateT' to track unique identifiers.
newtype GWidget sub master a = GWidget (
    WriterT (Body (Route master)) (
    WriterT (Last Title) (
    WriterT (UniqueList (Script (Route master))) (
    WriterT (UniqueList (Stylesheet (Route master))) (
    WriterT (Maybe (Camlet (Route master))) (
    WriterT (Maybe (Jamlet (Route master))) (
    WriterT (Head (Route master)) (
    StateT Int (
    GHandler sub master
    )))))))) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadCatchIO)
instance Monoid (GWidget sub master ()) where
    mempty = return ()
    mappend x y = x >> y
-- | A 'GWidget' specialized to when the subsite and master site are the same.
type Widget y = GWidget y y

-- | Lift an action in the 'GHandler' monad into an action in the 'GWidget'
-- monad.
liftHandler :: GHandler sub master a -> GWidget sub master a
liftHandler = GWidget . lift . lift . lift . lift . lift . lift . lift . lift

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitle :: Html () -> GWidget sub master ()
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
addStyle :: Camlet (Route master) -> GWidget sub master ()
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
addJavaScript :: Jamlet (Route master) -> GWidget sub master ()
addJavaScript = GWidget . lift . lift . lift . lift . lift. tell . Just

-- | Apply the default layout to the given widget.
applyLayoutW :: (Eq (Route m), Yesod m)
             => GWidget sub m () -> GHandler sub m RepHtml
applyLayoutW w = widgetToPageContent w >>= fmap RepHtml . defaultLayout

-- | Convert a widget to a 'PageContent'.
widgetToPageContent :: Eq (Route master)
                    => GWidget sub master ()
                    -> GHandler sub master (PageContent (Route master))
widgetToPageContent (GWidget w) = do
    w' <- flip evalStateT 0
        $ runWriterT $ runWriterT $ runWriterT $ runWriterT
        $ runWriterT $ runWriterT $ runWriterT w
    let ((((((((),
         Body body),
         Last mTitle),
         scripts'),
         stylesheets'),
         style),
         jscript),
         Head head') = w'
    let title = maybe mempty unTitle mTitle
    let scripts = map (locationToHamlet . unScript) $ runUniqueList scripts'
    let stylesheets = map (locationToHamlet . unStylesheet)
                    $ runUniqueList stylesheets'
    -- FIXME the next functions can be optimized once blaze-html switches to
    -- blaze-builder
    let lbsToHtml = unsafeByteString . S.concat . L.toChunks
    let celper :: Camlet url -> Hamlet url
        celper c render = lbsToHtml $ renderCamlet render c
    let jelper :: Jamlet url -> Hamlet url
        jelper j render = lbsToHtml $ renderJamlet render j

    let head'' = [$hamlet|
$forall scripts s
    %script!src=^s^
$forall stylesheets s
    %link!rel=stylesheet!href=^s^
$maybe style s
    %style ^celper.s^
$maybe jscript j
    %script ^jelper.j^
^head'^
|]
    return $ PageContent title head'' body

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
