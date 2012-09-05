module Yesod.Internal.Session
    ( encodeClientSession
    , decodeClientSession
    , BackendSession
    , SaveSession
    , SessionBackend(..)
    ) where

import Yesod.Internal (Header(..))
import qualified Web.ClientSession as CS
import Data.Int (Int64)
import Data.Serialize
import Data.Time
import Data.ByteString (ByteString)
import Control.Monad (guard)
import Data.Text (Text, pack, unpack)
import Control.Arrow (first)
import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as S8
import qualified Network.Wai as W

type BackendSession = [(Text, S8.ByteString)]

type SaveSession = BackendSession    -- ^ The session contents after running the handler
                -> UTCTime           -- ^ current time
                -> IO [Header]

newtype SessionBackend master = SessionBackend
    { sbLoadSession :: master
                    -> W.Request
                    -> UTCTime
                    -> IO (BackendSession, SaveSession) -- ^ Return the session data and a function to save the session
    }

encodeClientSession :: CS.Key
                    -> CS.IV
                    -> UTCTime -- ^ expire time
                    -> ByteString -- ^ remote host
                    -> [(Text, ByteString)] -- ^ session
                    -> ByteString -- ^ cookie value
encodeClientSession key iv expire rhost session' =
    CS.encrypt key iv $ encode $ SessionCookie expire rhost session'

decodeClientSession :: CS.Key
                    -> UTCTime -- ^ current time
                    -> ByteString -- ^ remote host field
                    -> ByteString -- ^ cookie value
                    -> Maybe [(Text, ByteString)]
decodeClientSession key now rhost encrypted = do
    decrypted <- CS.decrypt key encrypted
    SessionCookie expire rhost' session' <-
        either (const Nothing) Just $ decode decrypted
    guard $ expire > now
    guard $ rhost' == rhost
    return session'

data SessionCookie = SessionCookie UTCTime ByteString [(Text, ByteString)]
    deriving (Show, Read)
instance Serialize SessionCookie where
    put (SessionCookie a b c) = putTime a >> put b >> put (map (first unpack) c)
    get = do
        a <- getTime
        b <- get
        c <- map (first pack) <$> get
        return $ SessionCookie a b c


----------------------------------------------------------------------


putTime :: Putter UTCTime
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
