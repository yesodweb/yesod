{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Internal.Core
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
      -- * Sessions
    , SessionBackend (..)
    , defaultClientSessionBackend
    , clientSessionBackend
    , loadClientSession
    , clientSessionDateCacher
      -- * jsLoader
    , ScriptLoadPosition (..)
    , BottomOfHeadAsync
    , loadJsYepnope
      -- * Misc
    , yesodVersion
    , yesodRender
    , resolveApproot
    , Approot (..)
    , FileUpload (..)
    , runFakeHandler
    ) where

import Yesod.Content
import Yesod.Handler hiding (lift, getExpires)

import Yesod.Routes.Class

import qualified Network.Wai as W
import Yesod.Internal.Session
import Yesod.Internal.Request
import Text.Hamlet
import Text.Blaze (unsafeLazyByteString)
import Data.Text (Text)
import Data.Aeson (Value (Array, String))
import Data.Aeson.Encode (encode)
import qualified Data.Vector as Vector
import qualified Paths_yesod_core
import Data.Version (showVersion)
import System.Log.FastLogger (Logger)
import Yesod.Core.Types
import Yesod.Core.Class
import Yesod.Core.Run

yesodVersion :: String
yesodVersion = showVersion Paths_yesod_core.version

-- | A type-safe, concise method of creating breadcrumbs for pages. For each
-- resource, you declare the title of the page and the parent resource (if
-- present).
class YesodBreadcrumbs y where
    -- | Returns the title and the parent resource, if available. If you return
    -- a 'Nothing', then this is considered a top-level page.
    breadcrumb :: Route y -> GHandler sub y (Text , Maybe (Route y))

-- | Gets the title of the current page and the hierarchy of parent pages,
-- along with their respective titles.
breadcrumbs :: YesodBreadcrumbs y => GHandler sub y (Text, [(Route y, Text)])
breadcrumbs = do
    x' <- getCurrentRoute
    tm <- getRouteToMaster
    let x = fmap tm x'
    case x of
        Nothing -> return ("Not found", [])
        Just y -> do
            (title, next) <- breadcrumb y
            z <- go [] next
            return (title, z)
  where
    go back Nothing = return back
    go back (Just this) = do
        (title, next) <- breadcrumb this
        go ((this, title) : back) next

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

jsonArray :: [Text] -> Html
jsonArray = unsafeLazyByteString . encode . Array . Vector.fromList . map String

-- | For use with setting 'jsLoader' to 'BottomOfHeadAsync'
loadJsYepnope :: Yesod master => Either Text (Route master) -> [Text] -> Maybe (HtmlUrl (Route master)) -> (HtmlUrl (Route master))
loadJsYepnope eyn scripts mcomplete =
  [hamlet|
$newline never
    $maybe yn <- left eyn
        <script src=#{yn}>
    $maybe yn <- right eyn
        <script src=@{yn}>
    $maybe complete <- mcomplete
        <script>yepnope({load:#{jsonArray scripts},complete:function(){^{complete}}});
    $nothing
        <script>yepnope({load:#{jsonArray scripts}});
|]

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class YesodDispatch sub master where
    yesodDispatch
        :: Yesod master
        => Logger
        -> master
        -> sub
        -> (Route sub -> Route master)
        -> (Maybe (SessionBackend master) -> W.Application) -- ^ 404 handler
        -> (Route sub -> Maybe (SessionBackend master) -> W.Application) -- ^ 405 handler
        -> Text -- ^ request method
        -> [Text] -- ^ pieces
        -> Maybe (SessionBackend master)
        -> W.Application

    yesodRunner :: Yesod master
                => Logger
                -> GHandler sub master ChooseRep
                -> master
                -> sub
                -> Maybe (Route sub)
                -> (Route sub -> Route master)
                -> Maybe (SessionBackend master)
                -> W.Application
    yesodRunner logger handler master sub murl tomaster msb = defaultYesodRunner YesodRunnerEnv
        { yreLogger = logger
        , yreMaster = master
        , yreSub = sub
        , yreRoute = murl
        , yreToMaster = tomaster
        , yreSessionBackend = msb
        } handler

instance YesodDispatch WaiSubsite master where
    yesodDispatch _logger _master (WaiSubsite app) _tomaster _404 _405 _method _pieces _session = app
