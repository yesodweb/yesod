module Yesod.Internal.Session
    ( encodeClientSession
    , decodeClientSession
    , clientSessionDateCacher
    , ClientSessionDateCache(..)
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
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forever, guard)
import Data.Text (Text, pack, unpack)
import Control.Arrow (first)
import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as S8
import qualified Data.IORef as I
import qualified Network.Wai as W

type BackendSession = [(Text, S8.ByteString)]

type SaveSession = BackendSession    -- ^ The session contents after running the handler
                -> IO [Header]

newtype SessionBackend master = SessionBackend
    { sbLoadSession :: master
                    -> W.Request
                    -> IO (BackendSession, SaveSession) -- ^ Return the session data and a function to save the session
    }

encodeClientSession :: CS.Key
                    -> CS.IV
                    -> ClientSessionDateCache  -- ^ expire time
                    -> ByteString -- ^ remote host
                    -> [(Text, ByteString)] -- ^ session
                    -> ByteString -- ^ cookie value
encodeClientSession key iv date rhost session' =
    CS.encrypt key iv $ encode $ SessionCookie expires rhost session'
      where expires = Right (csdcExpiresSerialized date)

decodeClientSession :: CS.Key
                    -> ClientSessionDateCache  -- ^ current time
                    -> ByteString -- ^ remote host field
                    -> ByteString -- ^ cookie value
                    -> Maybe [(Text, ByteString)]
decodeClientSession key date rhost encrypted = do
    decrypted <- CS.decrypt key encrypted
    SessionCookie (Left expire) rhost' session' <-
        either (const Nothing) Just $ decode decrypted
    guard $ expire > csdcNow date
    guard $ rhost' == rhost
    return session'

data SessionCookie = SessionCookie (Either UTCTime ByteString) ByteString [(Text, ByteString)]
    deriving (Show, Read)
instance Serialize SessionCookie where
    put (SessionCookie a b c) = do
        either putTime putByteString a
        put b
        put (map (first unpack) c)
    get = do
        a <- getTime
        b <- get
        c <- map (first pack) <$> get
        return $ SessionCookie (Left a) b c


----------------------------------------------------------------------


-- Mostly copied from Kazu's date-cache, but with modifications
-- that better suit our needs.
--
-- The cached date is updated every 10s, we don't need second
-- resolution for session expiration times.

data ClientSessionDateCache =
  ClientSessionDateCache {
    csdcNow               :: !UTCTime
  , csdcExpires           :: !UTCTime
  , csdcExpiresSerialized :: !ByteString
  } deriving (Eq, Show)

clientSessionDateCacher ::
     NominalDiffTime -- ^ Inactive session valitity.
  -> IO (IO ClientSessionDateCache, IO ())
clientSessionDateCacher validity = do
    ref <- getUpdated >>= I.newIORef
    tid <- forkIO $ forever (doUpdate ref)
    return $! (I.readIORef ref, killThread tid)
  where
    getUpdated = do
      now <- getCurrentTime
      let expires  = validity `addUTCTime` now
          expiresS = runPut (putTime expires)
      return $! ClientSessionDateCache now expires expiresS
    doUpdate ref = do
      threadDelay 10000000 -- 10s
      I.writeIORef ref =<< getUpdated


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
