{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
-- | Widgets combine HTML with JS and CSS dependencies with a unique identifier
-- generator, allowing you to create truly modular HTML components.
module Yesod.Core.Widget
    ( -- * Datatype
      GWidget
    , PageContent (..)
      -- * Special Hamlet quasiquoter/TH for Widgets
    , whamlet
    , whamletFile
    , ihamletToRepHtml
      -- * Convert to Widget
    , ToWidget (..)
    , ToWidgetHead (..)
    , ToWidgetBody (..)
    , ToWidgetMedia (..)
      -- * Creating
      -- ** Head of page
    , setTitle
    , setTitleI
      -- ** Body
    , addSubWidget
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
      -- * Internal
    , unGWidget
    , whamletFileWithSettings
    ) where

import Data.Monoid
import qualified Text.Blaze.Html5 as H
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Yesod.Routes.Class
import Yesod.Core.Handler (getMessageRender, getUrlRenderParams)
import Yesod.Core.Class.MonadLift (lift)
import Text.Shakespeare.I18N (RenderMessage)
import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Map as Map
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp (InfixE, VarE, LamE, AppE), Pat (VarP), newName)

import qualified Text.Hamlet as NP
import Data.Text.Lazy.Builder (fromLazyText)
import Text.Blaze.Html (toHtml, preEscapedToMarkup)
import qualified Data.Text.Lazy as TL

import Yesod.Core.Types

preEscapedLazyText :: TL.Text -> Html
preEscapedLazyText = preEscapedToMarkup

addSubWidget :: (Route sub -> Route master) -> sub -> GWidget sub master a -> GWidget sub' master a
addSubWidget toMaster sub (GWidget (GHandler f)) =
    GWidget $ GHandler $ f . modHD
  where
    modHD hd = hd
        { handlerEnv = (handlerEnv hd)
            { rheRoute = Nothing
            , rheSub = sub
            , rheToMaster = toMaster
            }
        }

class ToWidget sub master a where
    toWidget :: a -> GWidget sub master ()

instance render ~ RY master => ToWidget sub master (render -> Html) where
    toWidget x = tell $ GWData (Body x) mempty mempty mempty mempty mempty mempty
instance render ~ RY master => ToWidget sub master (render -> Css) where
    toWidget x = toWidget $ CssBuilder . fromLazyText . renderCss . x
instance render ~ RY master => ToWidget sub master (render -> CssBuilder) where
    toWidget x = tell $ GWData mempty mempty mempty mempty (Map.singleton Nothing $ unCssBuilder . x) mempty mempty
instance render ~ RY master => ToWidget sub master (render -> Javascript) where
    toWidget x = tell $ GWData mempty mempty mempty mempty mempty (Just x) mempty
instance (sub' ~ sub, master' ~ master) => ToWidget sub' master' (GWidget sub master ()) where
    toWidget = id
instance ToWidget sub master Html where
    toWidget = toWidget . const

-- | Allows adding some CSS to the page with a specific media type.
--
-- Since 1.2
class ToWidgetMedia sub master a where
    -- | Add the given content to the page, but only for the given media type.
    --
    -- Since 1.2
    toWidgetMedia :: Text -- ^ media value
                  -> a
                  -> GWidget sub master ()
instance render ~ RY master => ToWidgetMedia sub master (render -> Css) where
    toWidgetMedia media x = toWidgetMedia media $ CssBuilder . fromLazyText . renderCss . x
instance render ~ RY master => ToWidgetMedia sub master (render -> CssBuilder) where
    toWidgetMedia media x = tell $ GWData mempty mempty mempty mempty (Map.singleton (Just media) $ unCssBuilder . x) mempty mempty

class ToWidgetBody sub master a where
    toWidgetBody :: a -> GWidget sub master ()

instance render ~ RY master => ToWidgetBody sub master (render -> Html) where
    toWidgetBody = toWidget
instance render ~ RY master => ToWidgetBody sub master (render -> Javascript) where
    toWidgetBody j = toWidget $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetBody sub master Html where
    toWidgetBody = toWidget

class ToWidgetHead sub master a where
    toWidgetHead :: a -> GWidget sub master ()

instance render ~ RY master => ToWidgetHead sub master (render -> Html) where
    toWidgetHead = tell . GWData mempty mempty mempty mempty mempty mempty . Head
instance render ~ RY master => ToWidgetHead sub master (render -> Css) where
    toWidgetHead = toWidget
instance render ~ RY master => ToWidgetHead sub master (render -> CssBuilder) where
    toWidgetHead = toWidget
instance render ~ RY master => ToWidgetHead sub master (render -> Javascript) where
    toWidgetHead j = toWidgetHead $ \r -> H.script $ preEscapedLazyText $ renderJavascriptUrl r j
instance ToWidgetHead sub master Html where
    toWidgetHead = toWidgetHead . const

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitle :: Html -> GWidget sub master ()
setTitle x = tell $ GWData mempty (Last $ Just $ Title x) mempty mempty mempty mempty mempty

-- | Set the page title. Calling 'setTitle' multiple times overrides previously
-- set values.
setTitleI :: RenderMessage master msg => msg -> GWidget sub master ()
setTitleI msg = do
    mr <- lift getMessageRender
    setTitle $ toHtml $ mr msg

-- | Link to the specified local stylesheet.
addStylesheet :: Route master -> GWidget sub master ()
addStylesheet = flip addStylesheetAttrs []

-- | Link to the specified local stylesheet.
addStylesheetAttrs :: Route master -> [(Text, Text)] -> GWidget sub master ()
addStylesheetAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Local x) y) mempty mempty mempty

-- | Link to the specified remote stylesheet.
addStylesheetRemote :: Text -> GWidget sub master ()
addStylesheetRemote = flip addStylesheetRemoteAttrs []

-- | Link to the specified remote stylesheet.
addStylesheetRemoteAttrs :: Text -> [(Text, Text)] -> GWidget sub master ()
addStylesheetRemoteAttrs x y = tell $ GWData mempty mempty mempty (toUnique $ Stylesheet (Remote x) y) mempty mempty mempty

addStylesheetEither :: Either (Route master) Text -> GWidget sub master ()
addStylesheetEither = either addStylesheet addStylesheetRemote

addScriptEither :: Either (Route master) Text -> GWidget sub master ()
addScriptEither = either addScript addScriptRemote

-- | Link to the specified local script.
addScript :: Route master -> GWidget sub master ()
addScript = flip addScriptAttrs []

-- | Link to the specified local script.
addScriptAttrs :: Route master -> [(Text, Text)] -> GWidget sub master ()
addScriptAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Local x) y) mempty mempty mempty mempty

-- | Link to the specified remote script.
addScriptRemote :: Text -> GWidget sub master ()
addScriptRemote = flip addScriptRemoteAttrs []

-- | Link to the specified remote script.
addScriptRemoteAttrs :: Text -> [(Text, Text)] -> GWidget sub master ()
addScriptRemoteAttrs x y = tell $ GWData mempty mempty (toUnique $ Script (Remote x) y) mempty mempty mempty mempty

whamlet :: QuasiQuoter
whamlet = NP.hamletWithSettings rules NP.defaultHamletSettings

whamletFile :: FilePath -> Q Exp
whamletFile = NP.hamletFileWithSettings rules NP.defaultHamletSettings

whamletFileWithSettings :: NP.HamletSettings -> FilePath -> Q Exp
whamletFileWithSettings = NP.hamletFileWithSettings rules

rules :: Q NP.HamletRules
rules = do
    ah <- [|toWidget|]
    let helper qg f = do
            x <- newName "urender"
            e <- f $ VarE x
            let e' = LamE [VarP x] e
            g <- qg
            bind <- [|(>>=)|]
            return $ InfixE (Just g) bind (Just e')
    let ur f = do
            let env = NP.Env
                    (Just $ helper [|liftW getUrlRenderParams|])
                    (Just $ helper [|liftM (toHtml .) $ liftW getMessageRender|])
            f env
    return $ NP.HamletRules ah ur $ \_ b -> return $ ah `AppE` b

-- | Wraps the 'Content' generated by 'hamletToContent' in a 'RepHtml'.
ihamletToRepHtml :: RenderMessage master message
                 => HtmlUrlI18n message (Route master)
                 -> GHandler sub master Html
ihamletToRepHtml ih = do
    urender <- getUrlRenderParams
    mrender <- getMessageRender
    return $ ih (toHtml . mrender) urender

tell :: GWData (Route master) -> GWidget sub master ()
tell w = GWidget $ return ((), w)

-- | Type-restricted version of @lift@. Used internally to create better error
-- messages.
liftW :: GHandler sub master a -> GWidget sub master a
liftW = lift

toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)
