{-# LANGUAGE CPP #-}
module Yesod.Core.Internal.Util
    ( putTime
    , getTime
    , formatW3
    , formatRFC1123
    , formatRFC822
    , getCurrentMaxExpiresRFC1123
    ) where

import           Data.Int       (Int64)
import           Data.Serialize (Get, Put, Serialize (..))
import qualified Data.Text      as T
import           Data.Time      (Day (ModifiedJulianDay, toModifiedJulianDay),
                                 DiffTime, UTCTime (..), formatTime,
                                 getCurrentTime, addUTCTime)
import           Control.Monad  (liftM)

#if MIN_VERSION_time(1,5,0)
import           Data.Time      (defaultTimeLocale)
#else
import           System.Locale  (defaultTimeLocale)
#endif

putTime :: UTCTime -> Put
putTime (UTCTime d t) =
  let d' = fromInteger  $ toModifiedJulianDay d
      t' = fromIntegral $ fromEnum (t / diffTimeScale)
  in put (d' * posixDayLength_int64 + min posixDayLength_int64 t')

getTime :: Get UTCTime
getTime = do
  val <- get
  let (d, t) = val `divMod` posixDayLength_int64
      d' = ModifiedJulianDay $! fromIntegral d
      t' = fromIntegral t
  d' `seq` t' `seq` return (UTCTime d' t')

posixDayLength_int64 :: Int64
posixDayLength_int64 = 86400

diffTimeScale :: DiffTime
diffTimeScale = 1e12

-- | Format a 'UTCTime' in W3 format.
formatW3 :: UTCTime -> T.Text
formatW3 = T.pack . formatTime defaultTimeLocale "%FT%X-00:00"

-- | Format as per RFC 1123.
formatRFC1123 :: UTCTime -> T.Text
formatRFC1123 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X %Z"

-- | Format as per RFC 822.
formatRFC822 :: UTCTime -> T.Text
formatRFC822 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z"

-- | Format the time a year from now as per RFC 1123.
getCurrentMaxExpiresRFC1123 :: IO T.Text
getCurrentMaxExpiresRFC1123 = liftM (formatRFC1123 . addUTCTime (60*60*24*365)) getCurrentTime
