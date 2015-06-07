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
    , defaultYesodMiddleware
    , authorizationCheck
      -- * Data types
    , AuthResult (..)
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
    , customizeSessionCookies
    , defaultClientSessionBackend
    , envClientSessionBackend
    , clientSessionBackend
    , sslOnlySessions
    , sslOnlyMiddleware
    , clientSessionDateCacher
    , loadClientSession
    , Header(..)
      -- * Subsites
    , MonadHandler (..)
    , MonadWidget (..)
    , getRouteToParent
    , defaultLayoutSub
      -- * Misc
    , yesodVersion
    , yesodRender
    , Yesod.Core.runFakeHandler
      -- * LiteApp
    , module Yesod.Core.Internal.LiteApp
      -- * Low-level
    , yesodRunner
      -- * Re-exports
    , module Yesod.Core.Content
    , module Yesod.Core.Dispatch
    , module Yesod.Core.Handler
    , module Yesod.Core.Widget
    , module Yesod.Core.Json
    , module Yesod.Core.Internal.Util
    , module Text.Blaze.Html
    , MonadTrans (..)
    , MonadIO (..)
    , MonadBase (..)
    , MonadBaseControl
    , MonadResource (..)
    , MonadLogger
      -- * Commonly referenced functions/datatypes
    , Application
      -- * Utilities
    , showIntegral
    , readIntegral
    -- FIXME: API breakage
    -- , unauthorizedI
    -- module Text.Shakesepare.I18N
    -- * Shakespeare
    -- ** Hamlet
    -- , hamlet
    -- , shamlet
    -- , xhamlet
    -- , HtmlUrl
      -- ** Julius
    -- , julius
    -- , JavascriptUrl
    -- , renderJavascriptUrl
      -- ** Cassius/Lucius
    -- , cassius
    -- , lucius
    -- , CssUrl
    -- , renderCssUrl
    ) where

import Yesod.Core.Content
import Yesod.Core.Dispatch
import Yesod.Core.Handler
import Yesod.Core.Class.Handler
import Yesod.Core.Json
import Yesod.Core.Types
import Yesod.Core.Internal.Util (formatW3 , formatRFC1123 , formatRFC822)
import Yesod.Core.Widget
import Text.Blaze.Html (Html, toHtml, preEscapedToMarkup)

import Control.Monad.Logger
import Control.Monad.Trans.Class (MonadTrans (..))
import Yesod.Core.Internal.Session
import Yesod.Core.Internal.Run (yesodRunner)
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Class.Breadcrumbs
import Yesod.Core.Internal.Run (yesodRender)
import qualified Yesod.Core.Internal.Run
import qualified Paths_yesod_core
import Data.Version (showVersion)
import Yesod.Routes.Class
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))

import Control.Monad.Trans.Resource (MonadResource (..))
import Yesod.Core.Internal.LiteApp
import Network.Wai (Application)

runFakeHandler :: (Yesod site, MonadIO m) =>
                  SessionMap
               -> (site -> Logger)
               -> site
               -> HandlerT site IO a
               -> m (Either ErrorResponse a)
runFakeHandler = Yesod.Core.Internal.Run.runFakeHandler
{-# DEPRECATED runFakeHandler "import runFakeHandler from Yesod.Core.Unsafe" #-}

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

showIntegral :: Integral a => a -> String
showIntegral x = show (fromIntegral x :: Integer)

readIntegral :: Num a => String -> Maybe a
readIntegral s =
    case reads s of
        (i, _):_ -> Just $ fromInteger i
        [] -> Nothing
