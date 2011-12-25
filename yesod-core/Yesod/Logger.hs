{-# LANGUAGE BangPatterns #-}
module Yesod.Logger
    ( Logger
    , makeLogger
    , makeDefaultLogger
    , flushLogger
    , timed
    , logText
    , logLazyText
    , logString
    , logBS
    , logMsg
    , formatLogText
    ) where

import System.IO (Handle, stdout, hFlush)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toChunks)
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as TLE
import System.Log.FastLogger
import Network.Wai.Logger.Date (DateRef, dateInit, getDate)

-- for timed logging
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Printf (printf)
import Data.Text (unpack)

-- for formatter
import Language.Haskell.TH.Syntax (Loc)
import Yesod.Core (LogLevel, fileLocationToString)

data Logger = Logger {
    loggerHandle  :: Handle
  , loggerDateRef :: DateRef
  }

makeLogger :: Handle -> IO Logger
makeLogger handle = dateInit >>= return . Logger handle

-- | uses stdout handle
makeDefaultLogger :: IO Logger
makeDefaultLogger = makeLogger stdout

flushLogger :: Logger -> IO ()
flushLogger = hFlush . loggerHandle

logMsg :: Logger -> [LogStr] -> IO ()
logMsg = hPutLogStr . loggerHandle

logLazyText :: Logger -> TL.Text -> IO ()
logLazyText logger msg = logMsg logger $
  map LB (toChunks $ TLE.encodeUtf8 msg) ++ [newLine]

logText :: Logger -> Text -> IO ()
logText logger = logBS logger . encodeUtf8

logBS :: Logger -> ByteString -> IO ()
logBS logger msg = logMsg logger [LB msg, newLine]

logString :: Logger -> String -> IO ()
logString logger msg = logMsg logger [LS msg, newLine]

formatLogText :: Logger -> Loc -> LogLevel -> Text -> IO [LogStr]
formatLogText logger loc level msg = formatLogMsg logger loc level (toLB msg)

toLB :: Text -> LogStr
toLB = LB . encodeUtf8

formatLogMsg :: Logger -> Loc -> LogLevel -> LogStr -> IO [LogStr]
formatLogMsg logger loc level msg = do
    date <- liftIO $ getDate $ loggerDateRef logger
    return
        [ LB date
        , LB $ pack" ["
        , LS (drop 5 $ show level)
        , LB $ pack "] "
        , msg
        , LB $ pack " @("
        , LS (fileLocationToString loc)
        , LB $ pack ") "
        ]

newLine :: LogStr
newLine = LB $ pack "\"\n"

-- | Execute a monadic action and log the duration
--
timed :: MonadIO m
      => Logger  -- ^ Logger
      -> Text  -- ^ Message
      -> m a     -- ^ Action
      -> m a     -- ^ Timed and logged action
timed logger msg action = do
    start <- liftIO getCurrentTime
    !result <- action
    stop <- liftIO getCurrentTime
    let diff = fromEnum $ diffUTCTime stop start
        ms = diff `div` 10 ^ (9 :: Int)
        formatted = printf "  [%4dms] %s" ms (unpack msg)
    liftIO $ logString logger formatted
    return result
