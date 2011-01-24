module Yesod.Internal.Session
    ( encodeSession
    , decodeSession
    ) where

import qualified Web.ClientSession as CS
import Data.Serialize
import Data.Time
import Data.ByteString (ByteString)
import Control.Monad (guard)

encodeSession :: CS.Key
              -> UTCTime -- ^ expire time
              -> ByteString -- ^ remote host
              -> [(String, String)] -- ^ session
              -> ByteString -- ^ cookie value
encodeSession key expire rhost session' =
    CS.encrypt key $ encode $ SessionCookie expire rhost session'

decodeSession :: CS.Key
              -> UTCTime -- ^ current time
              -> ByteString -- ^ remote host field
              -> ByteString -- ^ cookie value
              -> Maybe [(String, String)]
decodeSession key now rhost encrypted = do
    decrypted <- CS.decrypt key encrypted
    SessionCookie expire rhost' session' <-
        either (const Nothing) Just $ decode decrypted
    guard $ expire > now
    guard $ rhost' == rhost
    return session'

data SessionCookie = SessionCookie UTCTime ByteString [(String, String)]
    deriving (Show, Read)
instance Serialize SessionCookie where
    put (SessionCookie a b c) = putTime a >> put b >> put c
    get = do
        a <- getTime
        b <- get
        c <- get
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
