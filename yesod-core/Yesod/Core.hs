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
    , BackendSession
    -- * JS loaders
    , loadJsYepnope
    , ScriptLoadPosition (..)
    , BottomOfHeadAsync
      -- * Misc
    , yesodVersion
    , yesodRender
    , runFakeHandler
      -- * Re-exports
    , module Yesod.Content
    , module Yesod.Dispatch
    , module Yesod.Handler
    , module Yesod.Request
    , module Yesod.Widget
    , module Yesod.Core.Json
    , module Text.Shakespeare.I18N
    ) where

import Yesod.Internal.Core
import Yesod.Internal (Header(..))
import Yesod.Content
import Yesod.Dispatch
import Yesod.Handler
import Yesod.Request
import Yesod.Widget
import Yesod.Core.Json
import Text.Shakespeare.I18N

import Control.Monad.Logger

-- | Return an 'Unauthorized' value, with the given i18n message.
unauthorizedI :: RenderMessage master msg => msg -> GHandler sub master AuthResult
unauthorizedI msg =do
    mr <- getMessageRender
    return $ Unauthorized $ mr msg
