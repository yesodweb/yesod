{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Widgets combine HTML with JS and CSS dependencies with a unique identifier
-- generator, allowing you to create truly modular HTML components.
module Yesod.Widget
    ( -- * Datatype
      GWidget
    , GGWidget (..)
    , PageContent (..)
      -- * Special Hamlet quasiquoter/TH for Widgets
    , YesodMessage (..)
    , getMessageRender
    , whamlet
    , whamletFile
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
import Yesod.Handler
    (Route, GHandler, GGHandler, YesodSubRoute(..), toMasterHandlerMaybe, getYesod)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Yesod.Internal
import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Map as Map
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE), Pat (VarP), newName)
import Yesod.Handler (getUrlRenderParams, getYesodSub)
import Yesod.Request (languages)

import Control.Monad.IO.Control (MonadControlIO)
import qualified Text.Hamlet.NonPoly as NP

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

class YesodMessage sub master where
    type Message sub master
    renderMessage :: sub
                  -> master
                  -> [Text] -- ^ languages
                  -> Message sub master
                  -> Html

getMessageRender :: (Monad mo, YesodMessage s m) => GGHandler s m mo (Message s m -> Html)
getMessageRender = do
    s <- getYesodSub
    m <- getYesod
    l <- languages
    return $ renderMessage s m l

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
                    (Just $ helper [|lift getMessageRender|])
            f env
    return $ NP.HamletRules ah ur
