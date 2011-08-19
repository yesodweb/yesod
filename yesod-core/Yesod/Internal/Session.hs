module Yesod.Internal.Session
    ( encodeSession
    , decodeSession
    ) where

import qualified Web.ClientSession as CS
import Data.Serialize
import Data.Time
import Data.ByteString (ByteString)
import Control.Monad (guard)
import Data.Text (Text, pack, unpack)
import Control.Arrow ((***))

encodeSession :: CS.Key
              -> CS.IV
              -> UTCTime -- ^ expire time
              -> ByteString -- ^ remote host
              -> [(Text, Text)] -- ^ session
              -> ByteString -- ^ cookie value
encodeSession key iv expire rhost session' =
    CS.encrypt key iv $ encode $ SessionCookie expire rhost session'

decodeSession :: CS.Key
              -> UTCTime -- ^ current time
              -> ByteString -- ^ remote host field
              -> ByteString -- ^ cookie value
              -> Maybe [(Text, Text)]
decodeSession key now rhost encrypted = do
    decrypted <- CS.decrypt key encrypted
    SessionCookie expire rhost' session' <-
        either (const Nothing) Just $ decode decrypted
    guard $ expire > now
    guard $ rhost' == rhost
    return session'

data SessionCookie = SessionCookie UTCTime ByteString [(Text, Text)]
    deriving (Show, Read)
instance Serialize SessionCookie where
    put (SessionCookie a b c) = putTime a >> put b >> put (map (unpack *** unpack) c)
    get = do
        a <- getTime
        b <- get
        c <- map (pack *** pack) `fmap` get
        return $ SessionCookie a b c

putTime :: Putter UTCTime
putTime t@(UTCTime d _) = do
    put $ toModifiedJulianDay d
    let ndt = diffUTCTime t $ UTCTime d 0
    put $ toRational ndt

getTime :: Get UTCTime
getTime = do
    d <- get
    ndt <- get
    return $ fromRational ndt `addUTCTime` UTCTime (ModifiedJulianDay d) 0
