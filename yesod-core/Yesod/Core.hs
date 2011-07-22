{-# LANGUAGE TemplateHaskell #-}
module Yesod.Core
    ( -- * Type classes
      Yesod (..)
    , YesodDispatch (..)
    , RenderRoute (..)
      -- ** Breadcrumbs
    , YesodBreadcrumbs (..)
    , breadcrumbs
      -- * Utitlities
    , maybeAuthorized
    , widgetToPageContent
      -- * Defaults
    , defaultErrorHandler
      -- * Data types
    , AuthResult (..)
      -- * Logging
    , LogLevel (..)
    , formatLogMessage
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logOther
      -- * Misc
    , yesodVersion
    , yesodRender
      -- * Re-exports
    , module Yesod.Content
    , module Yesod.Dispatch
    , module Yesod.Handler
    , module Yesod.Request
    , module Yesod.Widget
    , module Yesod.Message
    ) where

import Yesod.Internal.Core
import Yesod.Content
import Yesod.Dispatch
import Yesod.Handler
import Yesod.Request
import Yesod.Widget
import Yesod.Message

import Language.Haskell.TH.Syntax
import Data.Text (Text)

logTH :: LogLevel -> Q Exp
logTH level =
    [|messageLoggerHandler $(qLocation >>= liftLoc) $(lift level)|]
  where
    liftLoc :: Loc -> Q Exp
    liftLoc (Loc a b c d e) = [|Loc $(lift a) $(lift b) $(lift c) $(lift d) $(lift e)|]

-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebug) "This is a debug log message"
logDebug :: Q Exp
logDebug = logTH LevelDebug

-- | See 'logDebug'
logInfo :: Q Exp
logInfo = logTH LevelInfo
-- | See 'logDebug'
logWarn :: Q Exp
logWarn = logTH LevelWarn
-- | See 'logDebug'
logError :: Q Exp
logError = logTH LevelError

-- | Generates a function that takes a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $(logOther "My new level") "This is a log message"
logOther :: Text -> Q Exp
logOther = logTH . LevelOther
