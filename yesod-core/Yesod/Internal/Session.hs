module Yesod.Internal.Session
    ( encodeClientSession
    , decodeClientSession
    , clientSessionDateCacher
    , ClientSessionDateCache(..)
    , SaveSession
    , SessionBackend(..)
    ) where

import qualified Web.ClientSession as CS
import Data.Serialize
import Data.Time
import Data.ByteString (ByteString)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forever, guard)
import Yesod.Core.Types
import Yesod.Core.Internal.Util
import qualified Data.IORef as I

encodeClientSession :: CS.Key
                    -> CS.IV
                    -> ClientSessionDateCache  -- ^ expire time
                    -> ByteString -- ^ remote host
                    -> SessionMap -- ^ session
                    -> ByteString -- ^ cookie value
encodeClientSession key iv date rhost session' =
    CS.encrypt key iv $ encode $ SessionCookie expires rhost session'
      where expires = Right (csdcExpiresSerialized date)

decodeClientSession :: CS.Key
                    -> ClientSessionDateCache  -- ^ current time
                    -> ByteString -- ^ remote host field
                    -> ByteString -- ^ cookie value
                    -> Maybe SessionMap
decodeClientSession key date rhost encrypted = do
    decrypted <- CS.decrypt key encrypted
    SessionCookie (Left expire) rhost' session' <-
        either (const Nothing) Just $ decode decrypted
    guard $ expire > csdcNow date
    guard $ rhost' == rhost
    return session'


----------------------------------------------------------------------


-- Mostly copied from Kazu's date-cache, but with modifications
-- that better suit our needs.
--
-- The cached date is updated every 10s, we don't need second
-- resolution for session expiration times.

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
