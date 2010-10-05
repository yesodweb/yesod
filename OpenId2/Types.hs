{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.OpenID.Types
-- Copyright   : (c) Trevor Elliott, 2008
-- License     : BSD3
--
-- Maintainer  : Trevor Elliott <trevor@geekgateway.com>
-- Stability   : 
-- Portability : 
--

module OpenId2.Types (
    AssocType(..)
  , SessionType(..)
  , Association(..)
  , Params
  , ReturnTo
  , Realm
  , Resolver
  , Provider (..)
  , parseProvider
  , showProvider
  , modifyProvider
  , Identifier(..)
  , Error(..)
  , assocString
  , OpenIdException (..)
  ) where

-- Libraries
import Data.List
import Data.Word
import Network.URI
import Network.HTTP.Enumerator (Request, Response)
import Control.Exception (Exception)
import Data.Typeable (Typeable)

data OpenIdException =
      NormalizationException String
    | DiscoveryException String
    | AuthenticationException String
  deriving (Show, Typeable)
instance Exception OpenIdException

--------------------------------------------------------------------------------
-- Types

-- | Supported association types
data AssocType = HmacSha1 | HmacSha256
    deriving (Read,Show)

assocString :: AssocType -> String
assocString HmacSha1 = "HMAC-SHA1"
assocString HmacSha256 = "HMAC-SHA256"

{-
instance Show AssocType where
    show HmacSha1 = "HMAC-SHA1"
    show HmacSha256 = "HMAC-SHA256"

instance Read AssocType where
  readsPrec _ str | "HMAC-SHA1"   `isPrefixOf` str = [(HmacSha1  ,drop  9  str)]
                  | "HMAC-SHA256" `isPrefixOf` str = [(HmacSha256, drop 11 str)]
                  | otherwise                      = []
-}

-- | Session types for association establishment
data SessionType = NoEncryption | DhSha1 | DhSha256

instance Show SessionType where
  show NoEncryption = "no-encryption"
  show DhSha1       = "DH-SHA1"
  show DhSha256     = "DH-SHA256"

instance Read SessionType where
  readsPrec _ str
    | "no-encryption" `isPrefixOf` str = [(NoEncryption, drop 13 str)]
    | "DH-SHA1"       `isPrefixOf` str = [(DhSha1, drop 7 str)]
    | "DH-SHA256"     `isPrefixOf` str = [(DhSha256, drop 9 str)]
    | otherwise                        = []


-- | An association with a provider.
data Association = Association
  { assocExpiresIn :: Int
  , assocHandle    :: String
  , assocMacKey    :: [Word8]
  , assocType      :: AssocType
  } deriving (Show,Read)


-- | Parameter lists for communication with the server
type Params = [(String,String)]

-- | A return to path
type ReturnTo = String

-- | A realm of uris for a provider to inform a user about
type Realm = String

-- | A way to resolve an HTTP request
type Resolver m = Request -> m Response

-- | An OpenID provider.
newtype Provider = Provider { providerURI :: URI } deriving (Eq,Show)

-- | Parse a provider
parseProvider :: String -> Maybe Provider
parseProvider  = fmap Provider . parseURI

-- | Show a provider
showProvider :: Provider -> String
showProvider (Provider uri) = uriToString (const "") uri []

-- | Modify the URI in a provider
modifyProvider :: (URI -> URI) -> Provider -> Provider
modifyProvider f (Provider uri) = Provider (f uri)

-- | A valid OpenID identifier.
newtype Identifier = Identifier { getIdentifier :: String }
  deriving (Eq,Show,Read)

-- | Errors
newtype Error = Error String deriving Show
