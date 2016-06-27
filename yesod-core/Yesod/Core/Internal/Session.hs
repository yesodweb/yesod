module Yesod.Core.Internal.Session
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
import Control.Monad (guard)
import Yesod.Core.Types
import Yesod.Core.Internal.Util
import Control.AutoUpdate

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


-- Originally copied from Kazu's date-cache, but now using mkAutoUpdate.
--
-- The cached date is updated every 10s, we don't need second
-- resolution for session expiration times.
--
-- The second component of the returned tuple used to be an action that
-- killed the updater thread, but is now a no-op that's just there
-- to preserve the type.

clientSessionDateCacher ::
     NominalDiffTime -- ^ Inactive session validity.
  -> IO (IO ClientSessionDateCache, IO ())
clientSessionDateCacher validity = do
    getClientSessionDateCache <- mkAutoUpdate defaultUpdateSettings
      { updateAction = getUpdated
      , updateFreq   = 10000000 -- 10s
      }

    return $! (getClientSessionDateCache, return ())
  where
    getUpdated = do
      now <- getCurrentTime
      let expires  = validity `addUTCTime` now
          expiresS = runPut (putTime expires)
      return $! ClientSessionDateCache now expires expiresS
