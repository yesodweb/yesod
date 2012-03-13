module Yesod.Internal.Session
    ( encodeClientSession
    , decodeClientSession
    ) where

import qualified Web.ClientSession as CS
import Data.Serialize
import Data.Time
import Data.ByteString (ByteString)
import Control.Monad (guard)
import Data.Text (Text, pack, unpack)
import Control.Arrow (first)
import Control.Applicative ((<$>))

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
