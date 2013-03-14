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
    , defaultLayoutT
    , MonadHandler (..)
    , HandlerReader (..)
    , HandlerState (..)
    , HandlerError (..)
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
import Yesod.Core.Class.Handler
import Data.IORef (readIORef, newIORef)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Yesod.Core.Widget
import Yesod.Core.Json
import Yesod.Core.Types
import Text.Shakespeare.I18N
import Yesod.Core.Internal.Util (formatW3 , formatRFC1123 , formatRFC822)

import Control.Monad.Logger
import Control.Monad.Trans.Resource (MonadResource, liftResourceT)
import Control.Monad.Trans.Class (MonadTrans)
import Yesod.Core.Internal.Session
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Class.Breadcrumbs
import Yesod.Core.Internal.Run (yesodRender, runFakeHandler)
import qualified Paths_yesod_core
import Data.Version (showVersion)
import Yesod.Routes.Class (RenderRoute (..))

-- | Return an 'Unauthorized' value, with the given i18n message.
unauthorizedI :: RenderMessage site msg => msg -> GHandler site AuthResult
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
                -> GHandler site (Maybe (Route site))
maybeAuthorized r isWrite = do
    x <- isAuthorized r isWrite
    return $ if x == Authorized then Just r else Nothing

defaultLayoutT :: ( HandlerSite m ~ sub
                  , Yesod (HandlerMaster m)
                  , MonadHandler m
                  )
               => GWidget sub ()
               -> m RepHtml
defaultLayoutT (GWidget (GHandler f)) = do
    hd <- askHandlerData
    ((), gwdata) <- liftResourceT $ f hd
    liftHandlerMaster $ defaultLayout $ GWidget $ return ((), renderGWData (rheRender $ handlerEnv hd) gwdata)

renderGWData :: (x -> [(Text, Text)] -> Text) -> GWData x -> GWData y
renderGWData render gwd = GWData
    { gwdBody = fixBody $ gwdBody gwd
    , gwdTitle = gwdTitle gwd
    , gwdScripts = fixUnique fixScript $ gwdScripts gwd
    , gwdStylesheets = fixUnique fixStyle $ gwdStylesheets gwd
    , gwdCss = fmap fixCss $ gwdCss gwd
    , gwdJavascript = fmap fixJS $ gwdJavascript gwd
    , gwdHead = fixHead $ gwdHead gwd
    }
  where
    fixBody (Body h) = Body $ const $ h render
    fixHead (Head h) = Head $ const $ h render

    fixUnique go (UniqueList f) = UniqueList (map go (f []) ++)

    fixScript (Script loc attrs) = Script (fixLoc loc) attrs
    fixStyle (Stylesheet loc attrs) = Stylesheet (fixLoc loc) attrs

    fixLoc (Local url) = Remote $ render url []
    fixLoc (Remote t) = Remote t

    fixCss f = const $ f render

    fixJS f = const $ f render
