{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Yesod.Core.Dispatch
    ( -- * Quasi-quoted routing
      parseRoutes
    , parseRoutesNoCheck
    , parseRoutesFile
    , parseRoutesFileNoCheck
    , mkYesod
    , mkYesodWith
      -- ** More fine-grained
    , mkYesodData
    , mkYesodSubData
    , mkYesodDispatch
    , mkYesodSubDispatch
      -- *** Helpers
    , getGetMaxExpires
      -- ** Path pieces
    , PathPiece (..)
    , PathMultiPiece (..)
    , Texts
      -- * Convert to WAI
    , toWaiApp
    , toWaiAppPlain
    , toWaiAppYre
    , warp
    , warpDebug
    , warpEnv
    , mkDefaultMiddlewares
    , defaultMiddlewaresNoLogging
      -- * WAI subsites
    , WaiSubsite (..)
    , WaiSubsiteWithAuth (..)
    ) where

import Prelude hiding (exp)
import Yesod.Core.Internal.TH

import Web.PathPieces

import qualified Network.Wai as W

import Data.ByteString.Lazy.Char8 ()

import Data.Text (Text)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Builder (byteString, toLazyByteString)
import Network.HTTP.Types (status301, status307)
import Yesod.Routes.Parse
import Yesod.Core.Types
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Internal.Run
import Text.Read (readMaybe)
import System.Environment (getEnvironment)
import qualified System.Random as Random
import Control.AutoUpdate (mkAutoUpdate, defaultUpdateSettings, updateAction, updateFreq)
import Yesod.Core.Internal.Util (getCurrentMaxExpiresRFC1123)

import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.MethodOverride
import System.Log.FastLogger (fromLogStr)

import qualified Network.Wai.Handler.Warp
import Control.Monad (when)
import qualified Paths_yesod_core
import Data.Version (showVersion)
import RIO

-- | Get a 'LogFunc' from the site, or create if needed. Returns an
-- @IORef@ with a finalizer to clean up when done.
makeLogFunc :: Yesod site => site -> IO (LogFunc, IORef ())
makeLogFunc site =
  case getLogFunc site of
    Just logFunc -> do
      ref <- newIORef ()
      pure (logFunc, ref)
    Nothing -> do
      (logFunc, cleanup) <- logOptionsHandle stderr False >>= newLogFunc
      ref <- newIORef ()
      _ <- mkWeakIORef ref cleanup
      pure (logFunc, ref)

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This function will provide no middlewares; if you want commonly
-- used middlewares, please use 'toWaiApp'.
toWaiAppPlain :: YesodDispatch site => site -> IO W.Application
toWaiAppPlain site = do
    (logFunc, cleanup) <- makeLogFunc site
    sb <- makeSessionBackend site
    getMaxExpires <- getGetMaxExpires
    return $ toWaiAppYre YesodRunnerEnv
            { yreLogFunc = logFunc
            , yreSite = site
            , yreSessionBackend = sb
            , yreGen = defaultGen
            , yreGetMaxExpires = getMaxExpires
            , yreCleanup = cleanup
            }

defaultGen :: IO Int
defaultGen = Random.getStdRandom Random.next

-- | Pure low level function to construct WAI application. Usefull
-- when you need not standard way to run your app, or want to embed it
-- inside another app.
--
-- @since 1.4.29
toWaiAppYre :: YesodDispatch site => YesodRunnerEnv site -> W.Application
toWaiAppYre yre req =
    case cleanPath site $ W.pathInfo req of
        Left pieces -> sendRedirect site pieces req
        Right pieces -> yesodDispatch yre req
            { W.pathInfo = pieces
            }
  where
    site = yreSite yre
    sendRedirect :: Yesod master => master -> [Text] -> W.Application
    sendRedirect y segments' env sendResponse =
         sendResponse $ W.responseLBS status
                [ ("Content-Type", "text/plain")
                , ("Location", BL.toStrict $ toLazyByteString dest')
                ] "Redirecting"
      where
        -- Ensure that non-GET requests get redirected correctly. See:
        -- https://github.com/yesodweb/yesod/issues/951
        status
            | W.requestMethod env == "GET" = status301
            | otherwise                    = status307

        dest = joinPath y (resolveApproot y env) segments' []
        dest' =
            if S.null (W.rawQueryString env)
                then dest
                else dest `mappend`
                     byteString (W.rawQueryString env)

-- | Same as 'toWaiAppPlain', but provides a default set of middlewares. This
-- set may change with future releases, but currently covers:
--
-- * Logging
--
-- * GZIP compression
--
-- * Automatic HEAD method handling
--
-- * Request method override with the _method query string parameter
--
-- * Accept header override with the _accept query string parameter
toWaiApp :: YesodDispatch site => site -> IO W.Application
toWaiApp site = do
    (logFunc, cleanup) <- makeLogFunc site
    toWaiAppLogger logFunc cleanup site

toWaiAppLogger
  :: YesodDispatch site
  => LogFunc
  -> IORef () -- ^ cleanup
  -> site
  -> IO W.Application
toWaiAppLogger logFunc cleanup site = do
    sb <- makeSessionBackend site
    getMaxExpires <- getGetMaxExpires
    let yre = YesodRunnerEnv
                { yreLogFunc = logFunc
                , yreSite = site
                , yreSessionBackend = sb
                , yreGen = defaultGen
                , yreGetMaxExpires = getMaxExpires
                , yreCleanup = cleanup
                }
    runRIO logFunc $ logInfoS "yesod-core" "Application launched"
    middleware <- mkDefaultMiddlewares logFunc
    return $ middleware $ toWaiAppYre yre

-- | A convenience method to run an application using the Warp webserver on the
-- specified port. Automatically calls 'toWaiApp'. Provides a default set of
-- middlewares. This set may change at any point without a breaking version
-- number. Currently, it includes:
--
-- If you need more fine-grained control of middlewares, please use 'toWaiApp'
-- directly.
--
-- Since 1.2.0
warp :: YesodDispatch site => Int -> site -> IO ()
warp port site = do
    (logFunc, cleanup) <- makeLogFunc site
    toWaiAppLogger logFunc cleanup site >>= Network.Wai.Handler.Warp.runSettings (
        Network.Wai.Handler.Warp.setPort port $
        Network.Wai.Handler.Warp.setServerName serverValue $
        Network.Wai.Handler.Warp.setOnException (\_ e ->
                when (shouldLog' e) $
                runRIO logFunc $
                logErrorS "yesod-core" $
                "Exception from Warp: " <> displayShow e)
        Network.Wai.Handler.Warp.defaultSettings)
  where
    shouldLog' = Network.Wai.Handler.Warp.defaultShouldDisplayException

serverValue :: S8.ByteString
serverValue = S8.pack $ concat
    [ "Warp/"
    , Network.Wai.Handler.Warp.warpVersion
    , " + Yesod/"
    , showVersion Paths_yesod_core.version
    , " (core)"
    ]

-- | A default set of middlewares.
--
-- Since 1.2.0
mkDefaultMiddlewares :: LogFunc -> IO W.Middleware
mkDefaultMiddlewares logFunc = do
    logWare <- mkRequestLogger def
        { destination = Network.Wai.Middleware.RequestLogger.Callback $
            runRIO logFunc .
            logInfoS "yesod-core" .
            displayBytesUtf8 .
            fromLogStr
        , outputFormat = Apache FromSocket
        }
    return $ logWare . defaultMiddlewaresNoLogging

-- | All of the default middlewares, excluding logging.
--
-- Since 1.2.12
defaultMiddlewaresNoLogging :: W.Middleware
defaultMiddlewaresNoLogging = acceptOverride . autohead . gzip def . methodOverride

-- | Deprecated synonym for 'warp'.
warpDebug :: YesodDispatch site => Int -> site -> IO ()
warpDebug = warp
{-# DEPRECATED warpDebug "Please use warp instead" #-}

-- | Runs your application using default middlewares (i.e., via 'toWaiApp'). It
-- reads port information from the PORT environment variable, as used by tools
-- such as Keter and the FP Complete School of Haskell.
--
-- Note that the exact behavior of this function may be modified slightly over
-- time to work correctly with external tools, without a change to the type
-- signature.
warpEnv :: YesodDispatch site => site -> IO ()
warpEnv site = do
    env <- getEnvironment
    case lookup "PORT" env of
        Nothing -> error "warpEnv: no PORT environment variable found"
        Just portS ->
            case readMaybe portS of
                Nothing -> error $ "warpEnv: invalid PORT environment variable: " ++ show portS
                Just port -> warp port site

-- | Default constructor for 'yreGetMaxExpires' field. Low level
-- function for simple manual construction of 'YesodRunnerEnv'.
--
-- @since 1.4.29
getGetMaxExpires :: IO (IO Text)
getGetMaxExpires = mkAutoUpdate defaultUpdateSettings
  { updateAction = getCurrentMaxExpiresRFC1123
  , updateFreq = 24 * 60 * 60 * 1000000 -- Update once per day
  }
