{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
-- | Widgets combine HTML with JS and CSS dependencies with a unique identifier
-- generator, allowing you to create truly modular HTML components.
module Yesod.Core.Widget
    ( -- * Datatype
      WidgetT(..)
    , PageContent (..)
      -- * Convert to Widget
    , ToWidget (..)
    , ToWidgetHead (..)
    , ToWidgetBody (..)
    , ToWidgetMedia (..)
      -- * Creating
      -- ** Head of page
    , setTitle
      -- ** CSS
    , addStylesheet
    , addStylesheetAttrs
    , addStylesheetRemote
    , addStylesheetRemoteAttrs
    , addStylesheetEither
    , CssBuilder (..)
      -- ** Javascript
    , addScript
    , addScriptAttrs
    , addScriptRemote
    , addScriptRemoteAttrs
    , addScriptEither
      -- * Subsites
    , widgetToParentWidget
    , handlerToWidget
      -- * Internal
    , asWidgetT

      -- * Formerly Yesod.Core.Types
    , ScriptLoadPosition(..)
    , BottomOfHeadAsync
    , GWData(..)
    , Head(..)
    , Body(..)

      -- * Formerly Yesod.Core.Class.Handler

      -- * Formerly Yesod.Core.Handler
      -- ** Streaming
    , sendChunkHtml
      -- ** Messages
    , setMessage
    , getMessage
      -- ** Hamlet
    , hamletToRepHtml

      -- * Formerly Yesod.Core.Json
    -- FIXME
    -- , defaultLayoutJson

      -- * Formerly Yesod.Core
    , MonadWidget (..)
    ) where

import           Control.Applicative                (Applicative(..))
import           Control.Monad                      (liftM, ap, forM)
import           Control.Monad.Base                 (MonadBase (liftBase))
import           Control.Monad.Catch                (MonadCatch (..))
import           Control.Monad.Catch                (MonadMask (..))
import           Control.Monad.Logger               (MonadLogger (..))
#if MIN_VERSION_monad_logger(0, 3, 10)
import Control.Monad.Logger (MonadLoggerIO (..))
#endif
import           Control.Monad.Reader               (MonadReader (..))
import           Control.Monad.Trans.Class          (MonadTrans (..))
import           Control.Monad.Trans.Control        (MonadBaseControl (..))
import           Control.Monad.Trans.Resource       (MonadResourceBase, MonadResource (..), runInternalState, MonadThrow (..))
import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )
import qualified Data.ByteString.Lazy               as L
import Blaze.ByteString.Builder (Builder)
import           Data.Conduit                       (Flush (Chunk), Producer, ConduitM)
import Data.Conduit.Internal (Pipe(..))
import Data.Conduit.Lazy (MonadActive, monadActive)
import           Data.List                          (foldl', nub)
import Data.Monoid
import Data.Semigroup (Semigroup)
import qualified Data.Text                          as T
import qualified Text.Blaze.Html.Renderer.Text as RenderText
import           Text.Blaze.Html               (preEscapedToMarkup, toHtml, Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Yesod.Routes.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Map as Map
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE, AppE), Pat (VarP), newName)

import Data.Text.Lazy.Builder (fromLazyText, toLazyText)
import           System.Log.FastLogger              (toLogStr)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding            (encodeUtf8)

import Yesod.Core.Types
import Yesod.Core.Class.Handler
import Yesod.Core.Class.Yesod (Yesod)
import Yesod.Core.Handler (getUrlRenderParams, toTextUrl, invalidArgs, permissionDenied, setSession, lookupSession, deleteSession, RedirectUrl, withUrlRenderer, sendChunk, getRequest, getYesod, sendResponse, selectRep, provideRep)
import Yesod.Core.Content (ToContent(..), ToTypedContent(..), HasContentType(..), ToFlushBuilder(..), typeJavascript, typeHtml, typeCss)
import           Data.Map                           (Map, unionWith)
import qualified Data.Aeson as J

-- templating types
class ToWidgetBuilder a where
  toWidgetBuilder :: a -> Builder

type Render url = url -> [(Text, Text)] -> Text
type Translate msg = msg -> Html
type HtmlUrl url = Render url -> Html
type HtmlUrlI18n msg url = Translate msg -> Render url -> Html



------------------------------------
-- Original Yesod.Core.Widget
------------------------------------
preEscapedLazyText :: TL.Text -> Html
preEscapedLazyText = preEscapedToMarkup

class ToWidget site a where
    toWidget :: (MonadWidget m, HandlerSite m ~ site) => a -> m ()

instance render ~ RY site => ToWidget site (render -> Html) where
    toWidget x = tell $ GWData (Body x) mempty mempty mempty mempty mempty mempty
instance render ~ RY site => ToWidget site (render -> CssBuilder) where
    toWidget x = tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing $ unCssBuilder . x) mempty mempty
instance ToWidget site CssBuilder where
    toWidget x = tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing $ unCssBuilder . const x) mempty mempty
instance (site' ~ site, IO ~ m, a ~ ()) => ToWidget site' (WidgetT site m a) where
    toWidget = liftWidgetT
instance ToWidget site Html where
    toWidget = toWidget . const

-- | Allows adding some CSS to the page with a specific media type.
--
-- Since 1.2
class ToWidgetMedia site a where
    -- | Add the given content to the page, but only for the given media type.
    --
    -- Since 1.2
    toWidgetMedia :: (MonadWidget m, HandlerSite m ~ site)
                  => Text -- ^ media value
                  -> a
                  -> m ()
instance render ~ RY site => ToWidgetMedia site (render -> CssBuilder) where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . x) mempty mempty
instance ToWidgetMedia site CssBuilder where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . const x) mempty mempty

class ToWidgetBody site a where
    toWidgetBody :: (MonadWidget m, HandlerSite m ~ site) => a -> m ()

instance render ~ RY site => ToWidgetBody site (render -> Html) where
    toWidgetBody = toWidget
instance ToWidgetBody site Html where
    toWidgetBody = toWidget

class ToWidgetHead site a where
    toWidgetHead :: (MonadWidget m, HandlerSite m ~ site) => a -> m ()

instance render ~ RY site => ToWidgetHead site (render -> Html) where
    toWidgetHead = tell . GWData mempty mempty mempty mempty mempty mempty . Head

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitle :: MonadWidget m => Html -> m ()
setTitle x = tell $ GWData mempty (Last $ Just $ Title x) mempty mempty mempty mempty mempty

-- | Link to the specified local stylesheet.
addStylesheet :: MonadWidget m => Route (HandlerSite m) -> m ()
addStylesheet = flip addStylesheetAttrs []

-- | Link to the specified local stylesheet.
addStylesheetAttrs :: MonadWidget m
                   => Route (HandlerSite m)
                   -> [(Text, Text)]
                   -> m ()
addStylesheetAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Local x) y) mempty mempty mempty

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: MonadWidget m => Text -> m ()
addStylesheetRemote = flip addStylesheetRemoteAttrs []

-- | Link to the specified remote stylesheet.
addStylesheetRemoteAttrs :: MonadWidget m => Text -> [(Text, Text)] -> m ()
addStylesheetRemoteAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Remote x) y) mempty mempty mempty

addStylesheetEither :: MonadWidget m
                    => Either (Route (HandlerSite m)) Text
                    -> m ()
addStylesheetEither = either addStylesheet addStylesheetRemote

addScriptEither :: MonadWidget m
                => Either (Route (HandlerSite m)) Text
                -> m ()
addScriptEither = either addScript addScriptRemote

-- | Link to the specified local script.
addScript :: MonadWidget m => Route (HandlerSite m) -> m ()
addScript = flip addScriptAttrs []

-- | Link to the specified local script.
addScriptAttrs :: MonadWidget m => Route (HandlerSite m) -> [(Text, Text)] -> m ()
addScriptAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Local x) y) mempty mempty mempty mempty

-- | Link to the specified remote script.
addScriptRemote :: MonadWidget m => Text -> m ()
addScriptRemote = flip addScriptRemoteAttrs []

-- | Link to the specified remote script.
addScriptRemoteAttrs :: MonadWidget m => Text -> [(Text, Text)] -> m ()
addScriptRemoteAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Remote x) y) mempty mempty mempty mempty

tell :: MonadWidget m => GWData (Route (HandlerSite m)) -> m ()
tell w = liftWidgetT $ WidgetT $ const $ return ((), w)

toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)

handlerToWidget :: Monad m => HandlerT site m a -> WidgetT site m a
handlerToWidget (HandlerT f) = WidgetT $ liftM (, mempty) . f

widgetToParentWidget :: MonadIO m
                     => WidgetT child IO a
                     -> HandlerT child (HandlerT parent m) (WidgetT parent m a)
widgetToParentWidget (WidgetT f) = HandlerT $ \hd -> do
    (a, gwd) <- liftIO $ f hd { handlerToParent = const () }
    return $ WidgetT $ const $ return (a, liftGWD (handlerToParent hd) gwd)

liftGWD :: (child -> parent) -> GWData child -> GWData parent
liftGWD tp gwd = GWData
    { gwdBody = fixBody $ gwdBody gwd
    , gwdTitle = gwdTitle gwd
    , gwdScripts = fixUnique fixScript $ gwdScripts gwd
    , gwdStylesheets = fixUnique fixStyle $ gwdStylesheets gwd
    , gwdCss = fmap fixCss $ gwdCss gwd
    , gwdJavascript = fmap fixJS $ gwdJavascript gwd
    , gwdHead = fixHead $ gwdHead gwd
    }
  where
    fixRender f route params = f (tp route) params

    fixBody (Body h) = Body $ h . fixRender
    fixHead (Head h) = Head $ h . fixRender

    fixUnique go (UniqueList f) = UniqueList (map go (f []) ++)

    fixScript (Script loc attrs) = Script (fixLoc loc) attrs
    fixStyle (Stylesheet loc attrs) = Stylesheet (fixLoc loc) attrs

    fixLoc (Local url) = Local $ tp url
    fixLoc (Remote t) = Remote t

    fixCss f = f . fixRender

    fixJS f = f . fixRender



------------------------------------
-- Formerly Yesod.Core.Types
------------------------------------
data GWData a = GWData
    { gwdBody        :: !(Body a)
    , gwdTitle       :: !(Last Title)
    , gwdScripts     :: !(UniqueList (Script a))
    , gwdStylesheets :: !(UniqueList (Stylesheet a))
    , gwdCss         :: !(Map (Maybe Text) (CssBuilderUrl a)) -- media type
    , gwdJavascript  :: !(Maybe (BuilderUrl a))
    , gwdHead        :: !(Head a)
    }
instance Monoid (GWData a) where
    mempty = GWData mempty mempty mempty mempty mempty mempty mempty
    mappend (GWData a1 a2 a3 a4 a5 a6 a7)
            (GWData b1 b2 b3 b4 b5 b6 b7) = GWData
        (a1 `mappend` b1)
        (a2 `mappend` b2)
        (a3 `mappend` b3)
        (a4 `mappend` b4)
        (unionWith mappend a5 b5)
        (a6 `mappend` b6)
        (a7 `mappend` b7)
instance Semigroup (GWData a)

-- Instances for WidgetT
instance Monad m => Functor (WidgetT site m) where
    fmap = liftM
instance Monad m => Applicative (WidgetT site m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (WidgetT site m) where
    return a = WidgetT $ const $ return (a, mempty)
    WidgetT x >>= f = WidgetT $ \r -> do
        (a, wa) <- x r
        (b, wb) <- unWidgetT (f a) r
        return (b, wa `mappend` wb)
instance MonadIO m => MonadIO (WidgetT site m) where
    liftIO = lift . liftIO
instance MonadBase b m => MonadBase b (WidgetT site m) where
    liftBase = WidgetT . const . liftBase . fmap (, mempty)
instance MonadBaseControl b m => MonadBaseControl b (WidgetT site m) where
#if MIN_VERSION_monad_control(1,0,0)
    type StM (WidgetT site m) a = StM m (a, GWData (Route site))
    liftBaseWith f = WidgetT $ \reader' ->
        liftBaseWith $ \runInBase ->
            liftM (\x -> (x, mempty))
            (f $ runInBase . flip unWidgetT reader')
    restoreM = WidgetT . const . restoreM
#else
    data StM (WidgetT site m) a = StW (StM m (a, GWData (Route site)))
    liftBaseWith f = WidgetT $ \reader' ->
        liftBaseWith $ \runInBase ->
            liftM (\x -> (x, mempty))
            (f $ liftM StW . runInBase . flip unWidgetT reader')
    restoreM (StW base) = WidgetT $ const $ restoreM base
#endif
instance Monad m => MonadReader site (WidgetT site m) where
    ask = WidgetT $ \hd -> return (rheSite $ handlerEnv hd, mempty)
    local f (WidgetT g) = WidgetT $ \hd -> g hd
        { handlerEnv = (handlerEnv hd)
            { rheSite = f $ rheSite $ handlerEnv hd
            }
        }

instance MonadTrans (WidgetT site) where
    lift = WidgetT . const . liftM (, mempty)
instance MonadThrow m => MonadThrow (WidgetT site m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (WidgetT site m) where
  catch (WidgetT m) c = WidgetT $ \r -> m r `catch` \e -> unWidgetT (c e) r
instance MonadMask m => MonadMask (WidgetT site m) where
  mask a = WidgetT $ \e -> mask $ \u -> unWidgetT (a $ q u) e
    where q u (WidgetT b) = WidgetT (u . b)
  uninterruptibleMask a =
    WidgetT $ \e -> uninterruptibleMask $ \u -> unWidgetT (a $ q u) e
      where q u (WidgetT b) = WidgetT (u . b)

instance (Applicative m, MonadIO m, MonadBase IO m, MonadThrow m) => MonadResource (WidgetT site m) where
    liftResourceT f = WidgetT $ \hd -> liftIO $ fmap (, mempty) $ runInternalState f (handlerResource hd)

instance MonadIO m => MonadLogger (WidgetT site m) where
    monadLoggerLog a b c d = WidgetT $ \hd ->
        liftIO $ fmap (, mempty) $ rheLog (handlerEnv hd) a b c (toLogStr d)

#if MIN_VERSION_monad_logger(0, 3, 10)
instance MonadIO m => MonadLoggerIO (WidgetT site m) where
    askLoggerIO = WidgetT $ \hd -> return (rheLog (handlerEnv hd), mempty)
#endif

instance MonadActive m => MonadActive (WidgetT site m) where
    monadActive = lift monadActive

data ScriptLoadPosition master
    = BottomOfBody
    | BottomOfHeadBlocking
    | BottomOfHeadAsync (BottomOfHeadAsync master)

type BottomOfHeadAsync master
       = [Text] -- ^ urls to load asynchronously
      -> Maybe (HtmlUrl (Route master)) -- ^ widget of js to run on async completion
      -> (HtmlUrl (Route master)) -- ^ widget to insert at the bottom of <head>

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. While this is simply a @WriterT@, we define a newtype for
-- better error messages.
newtype WidgetT site m a = WidgetT
    { unWidgetT :: HandlerData site (MonadRoute m) -> m (a, GWData (Route site))
    }

instance (a ~ (), Monad m) => Monoid (WidgetT site m a) where
    mempty = return ()
    mappend x y = x >> y
instance (a ~ (), Monad m) => Semigroup (WidgetT site m a)

asWidgetT :: WidgetT site m () -> WidgetT site m ()
asWidgetT = id


-- | Content for a web page. By providing this datatype, we can easily create
-- generic site templates, which would have the type signature:
--
-- > PageContent url -> HtmlUrl url
data PageContent url = PageContent
    { pageTitle :: Html
    , pageHead  :: HtmlUrl url
    , pageBody  :: HtmlUrl url
    }

newtype Head url = Head (HtmlUrl url)
    deriving Monoid
instance Semigroup (Head a)
newtype Body url = Body (HtmlUrl url)
    deriving Monoid
instance Semigroup (Body a)



------------------------------------
-- Formerly Yesod.Core.Content
------------------------------------
instance ToContent Html where
    toContent bs = ContentBuilder (renderHtmlBuilder bs) Nothing
instance ToTypedContent Html where
    toTypedContent h = TypedContent typeHtml (toContent h)
instance ToFlushBuilder (Flush Html) where toFlushBuilder = fmap renderHtmlBuilder
instance ToFlushBuilder Html where toFlushBuilder = Chunk . renderHtmlBuilder

instance HasContentType Html where
    getContentType _ = typeHtml

------------------------------------
-- Formerly Yesod.Core.Class.Handler
------------------------------------
replaceToParent :: HandlerData site route -> HandlerData site ()
replaceToParent hd = hd { handlerToParent = const () }

instance MonadResourceBase m => MonadHandler (WidgetT site m) where
    type HandlerSite (WidgetT site m) = site
    liftHandlerT (HandlerT f) = WidgetT $ liftIO . liftM (, mempty) . f . replaceToParent
{-# RULES "liftHandlerT (WidgetT site IO)" forall f. liftHandlerT (HandlerT f) = WidgetT $ liftM (, mempty) . f #-}

class MonadHandler m => MonadWidget m where
    liftWidgetT :: WidgetT (HandlerSite m) IO a -> m a
instance MonadResourceBase m => MonadWidget (WidgetT site m) where
    liftWidgetT (WidgetT f) = WidgetT $ liftIO . f . replaceToParent

#define GO(T) instance MonadWidget m => MonadWidget (T m) where liftWidgetT = lift . liftWidgetT
#define GOX(X, T) instance (X, MonadWidget m) => MonadWidget (T m) where liftWidgetT = lift . liftWidgetT
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(StateT s)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
GO(Pipe l i o u)
GO(ConduitM i o)
#undef GO
#undef GOX


------------------------------------
-- Formerly Yesod.Core.Handler
------------------------------------
msgKey :: Text
msgKey = T.pack "_MSG"
-- | Sets a message in the user's session.
--
-- See 'getMessage'.
setMessage :: MonadHandler m => Html -> m ()
setMessage = setSession msgKey . T.concat . TL.toChunks . RenderText.renderHtml

-- | Gets the message in the user's session, if available, and then clears the
-- variable.
--
-- See 'setMessage'.
getMessage :: MonadHandler m => m (Maybe Html)
getMessage = do
    mmsg <- liftM (fmap preEscapedToMarkup) $ lookupSession msgKey
    deleteSession msgKey
    return mmsg

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
hamletToRepHtml :: MonadHandler m => HtmlUrl (Route (HandlerSite m)) -> m Html
hamletToRepHtml = withUrlRenderer
{-# DEPRECATED hamletToRepHtml "Use withUrlRenderer instead" #-}

-- | Type-specialized version of 'sendChunk' for @Html@s.
--
-- Since 1.2.0
sendChunkHtml :: Monad m => Html -> Producer m (Flush Builder)
sendChunkHtml = sendChunk


-----------------------------
-- originally from Yesod.Core.Json
-----------------------------
-- | Provide both an HTML and JSON representation for a piece of
-- data, using the default layout for the HTML output
-- ('defaultLayout').
--
-- /Since: 0.3.0/
{- FIXME
defaultLayoutJson :: (Yesod site, J.ToJSON a)
                  => WidgetT site IO ()  -- ^ HTML
                  -> HandlerT site IO a  -- ^ JSON
                  -> HandlerT site IO TypedContent
defaultLayoutJson w json = selectRep $ do
    provideRep $ defaultLayout w
    provideRep $ fmap J.toJSON json
    -}
