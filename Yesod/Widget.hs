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
    , addJavascript
      -- * Manipulating
    , wrapWidget
    , extractBody
    ) where

import Data.List (nub)
import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Yesod.Hamlet (PageContent (..))
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Yesod.Handler (Route, GHandler, getUrlRenderParams)
import Yesod.Yesod (Yesod, defaultLayout, addStaticContent)
import Yesod.Content (RepHtml (..))
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO)

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
newtype Title = Title { unTitle :: Html }
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
-- | A 'GWidget' specialized to when the subsite and master site are the same.
type Widget y = GWidget y y

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

-- | Apply the default layout to the given widget.
applyLayoutW :: (Eq (Route m), Yesod m)
             => GWidget sub m () -> GHandler sub m RepHtml
applyLayoutW w = widgetToPageContent w >>= fmap RepHtml . defaultLayout

-- | Convert a widget to a 'PageContent'.
widgetToPageContent :: (Eq (Route master), Yesod master)
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
    let cssToHtml (Css b) = Html b
        celper :: Cassius url -> Hamlet url
        celper = fmap cssToHtml
        jsToHtml (Javascript b) = Html b
        jelper :: Julius url -> Hamlet url
        jelper = fmap jsToHtml

    render <- getUrlRenderParams
    let renderLoc x =
            case x of
                Nothing -> Nothing
                Just (Left s) -> Just s
                Just (Right (u, p)) -> Just $ render u p
    cssLoc <-
        case style of
            Nothing -> return Nothing
            Just s -> do
                x <- addStaticContent "css" "text/css; charset=utf-8"
                   $ renderCassius render s
                return $ renderLoc x
    jsLoc <-
        case jscript of
            Nothing -> return Nothing
            Just s -> do
                x <- addStaticContent "js" "text/javascript; charset=utf-8"
                   $ renderJulius render s
                return $ renderLoc x

    let head'' = [$hamlet|
$forall scripts s
    %script!src=^s^
$forall stylesheets s
    %link!rel=stylesheet!href=^s^
$maybe style s
    $maybe cssLoc s
        %link!rel=stylesheet!href=$s$
    $nothing
        %style ^celper.s^
$maybe jscript j
    $maybe jsLoc s
        %script!src=$s$
    $nothing
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
