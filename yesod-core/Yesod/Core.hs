{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Yesod.Core
    ( -- * Type classes
      Yesod (..)
    , YesodDispatch (..)
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
    , module Yesod.Core.Class.MonadLift
    , module Text.Shakespeare.I18N
    , module Yesod.Core.Internal.Util
    ) where

import Yesod.Core.Content
import Yesod.Core.Dispatch
import Yesod.Core.Handler
import Yesod.Core.Widget
import Yesod.Core.Json
import Yesod.Core.Types
import Yesod.Core.Class.MonadLift
import Text.Shakespeare.I18N
import Yesod.Core.Internal.Util (formatW3 , formatRFC1123 , formatRFC822)

import Control.Monad.Logger
import Yesod.Core.Internal.Session
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Class.Breadcrumbs
import Yesod.Core.Internal.Run (yesodRender, runFakeHandler)
import qualified Paths_yesod_core
import Data.Version (showVersion)
import Yesod.Routes.Class (RenderRoute (..))

-- | Return an 'Unauthorized' value, with the given i18n message.
unauthorizedI :: RenderMessage master msg => msg -> GHandler sub master AuthResult
unauthorizedI msg =do
    mr <- getMessageRender
    return $ Unauthorized $ mr msg

yesodVersion :: String
yesodVersion = showVersion Paths_yesod_core.version

-- | Return the same URL if the user is authorized to see it.
--
-- Built on top of 'isAuthorized'. This is useful for building page that only
-- contain links to pages the user is allowed to see.
maybeAuthorized :: Yesod a
                => Route a
                -> Bool -- ^ is this a write request?
                -> GHandler s a (Maybe (Route a))
maybeAuthorized r isWrite = do
    x <- isAuthorized r isWrite
    return $ if x == Authorized then Just r else Nothing
