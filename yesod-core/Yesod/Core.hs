{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Yesod.Core
    ( -- * Type classes
      Yesod (..)
    , YesodDispatch (..)
    , YesodSubDispatch (..)
    , RenderRoute (..)
    , ParseRoute (..)
    , RouteAttrs (..)
      -- ** Breadcrumbs
    , YesodBreadcrumbs (..)
    , breadcrumbs
      -- * Types
    , Approot (..)
    , FileUpload (..)
    , ErrorResponse (..)
      -- * Utitlities
    , maybeAuthorized
    , widgetToPageContent
      -- * Defaults
    , defaultErrorHandler
      -- * Data types
    , AuthResult (..)
    , unauthorizedI
      -- * Logging
    , LogLevel (..)
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logOther
    , logDebugS
    , logInfoS
    , logWarnS
    , logErrorS
    , logOtherS
      -- * Sessions
    , SessionBackend (..)
    , defaultClientSessionBackend
    , clientSessionBackend
    , clientSessionDateCacher
    , loadClientSession
    , Header(..)
    -- * JS loaders
    , ScriptLoadPosition (..)
    , BottomOfHeadAsync
      -- * Subsites
    , MonadHandler (..)
    , MonadWidget (..)
    , getRouteToParent
    , defaultLayoutSub
      -- * Misc
    , yesodVersion
    , yesodRender
    , runFakeHandler
      -- * LiteApp
    , module Yesod.Core.Internal.LiteApp
      -- * Re-exports
    , module Yesod.Core.Content
    , module Yesod.Core.Dispatch
    , module Yesod.Core.Handler
    , module Yesod.Core.Widget
    , module Yesod.Core.Json
    , module Text.Shakespeare.I18N
    , module Yesod.Core.Internal.Util
    , module Text.Blaze.Html
    , MonadTrans (..)
    , MonadIO (..)
    , MonadBase (..)
    , MonadBaseControl
    , MonadResource (..)
    , MonadLogger
    ) where

import Yesod.Core.Content
import Yesod.Core.Dispatch
import Yesod.Core.Handler
import Yesod.Core.Class.Handler
import Yesod.Core.Widget
import Yesod.Core.Json
import Yesod.Core.Types
import Text.Shakespeare.I18N
import Yesod.Core.Internal.Util (formatW3 , formatRFC1123 , formatRFC822)
import Text.Blaze.Html (Html, toHtml, preEscapedToMarkup)

import Control.Monad.Logger
import Control.Monad.Trans.Class (MonadTrans (..))
import Yesod.Core.Internal.Session
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Class.Breadcrumbs
import Yesod.Core.Internal.Run (yesodRender, runFakeHandler)
import qualified Paths_yesod_core
import Data.Version (showVersion)
import Yesod.Routes.Class
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))

import Control.Monad.Trans.Resource (MonadResource (..))
import Yesod.Core.Internal.LiteApp

-- | Return an 'Unauthorized' value, with the given i18n message.
unauthorizedI :: (MonadHandler m, RenderMessage (HandlerSite m) msg) => msg -> m AuthResult
unauthorizedI msg = do
    mr <- getMessageRender
    return $ Unauthorized $ mr msg

yesodVersion :: String
yesodVersion = showVersion Paths_yesod_core.version

-- | Return the same URL if the user is authorized to see it.
--
-- Built on top of 'isAuthorized'. This is useful for building page that only
-- contain links to pages the user is allowed to see.
maybeAuthorized :: Yesod site
                => Route site
                -> Bool -- ^ is this a write request?
                -> HandlerT site IO (Maybe (Route site))
maybeAuthorized r isWrite = do
    x <- isAuthorized r isWrite
    return $ if x == Authorized then Just r else Nothing

getRouteToParent :: Monad m => HandlerT child (HandlerT parent m) (Route child -> Route parent)
getRouteToParent = HandlerT $ return . handlerToParent

defaultLayoutSub :: Yesod parent
                 => WidgetT child IO ()
                 -> HandlerT child (HandlerT parent IO) Html
defaultLayoutSub cwidget = widgetToParentWidget cwidget >>= lift . defaultLayout
