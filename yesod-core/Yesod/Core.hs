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
    , HandlerReader (..)
    , HandlerState (..)
    , HandlerError (..)
    , getRouteToParent
    , liftDefaultLayout
      -- * Misc
    , yesodVersion
    , yesodRender
    , runFakeHandler
      -- * Re-exports
    , module Yesod.Core.Content
    , module Yesod.Core.Dispatch
    , module Yesod.Core.Handler
    , module Yesod.Core.Widget
    , module Yesod.Core.Json
    , module Text.Shakespeare.I18N
    , module Yesod.Core.Internal.Util
    , lift
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
import Text.Blaze.Html (Html)

import Control.Monad.Logger
import Control.Monad.Trans.Class (lift)
import Yesod.Core.Internal.Session
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Class.Breadcrumbs
import Yesod.Core.Internal.Run (yesodRender, runFakeHandler)
import qualified Paths_yesod_core
import Data.Version (showVersion)
import Yesod.Routes.Class (RenderRoute (..))

-- | Return an 'Unauthorized' value, with the given i18n message.
unauthorizedI :: (Monad m, RenderMessage site msg) => msg -> HandlerT site m AuthResult
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

-- | Get a function to convert a child site route to a parent site route.
--
-- Before Yesod 1.2, the @getRouteToMaster@ function did much the same thing.
-- The difference here is that @getRouteToParent@ will convert only one layer
-- in the subsite stack. In the common case of single-layer subsites,
-- therefore, these two functions are identical.
--
-- Since 1.2.0
getRouteToParent :: Monad m => HandlerT child (HandlerT parent m) (Route child -> Route parent)
getRouteToParent = HandlerT $ return . handlerToParent

-- | A common idiom for writing subsites is to create a widget, convert all
-- routes in the widget to the parent site, pass it to 'defaultLayout', and
-- then 'lift' into the child monad transformer. This function automates that
-- process.
--
-- Since 1.2.0
liftDefaultLayout :: Yesod parent
                  => WidgetT child IO ()
                  -> HandlerT child (HandlerT parent IO) Html
liftDefaultLayout w = liftWidget w >>= lift . defaultLayout
