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
    Provider (..)
  , Identifier (..)
  , AuthenticateException (..)
  ) where

-- Libraries
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Web.Authenticate.Internal

-- | An OpenID provider.
newtype Provider = Provider { providerURI :: String } deriving (Eq,Show)

-- | A valid OpenID identifier.
newtype Identifier = Identifier { identifier :: String }
  deriving (Eq, Show, Read)
