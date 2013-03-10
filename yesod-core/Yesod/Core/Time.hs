module Yesod.Core.Time
    ( putTime
    , getTime
    ) where

import           Data.Int       (Int64)
import           Data.Serialize (Get, Put, Serialize (..))
import           Data.Time      (Day (ModifiedJulianDay, toModifiedJulianDay),
                                 DiffTime, UTCTime (..))

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
