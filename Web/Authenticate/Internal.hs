{-# LANGUAGE DeriveDataTypeable #-}
module Web.Authenticate.Internal
    ( AuthenticateException (..)
    ) where

import Numeric (showHex)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

data AuthenticateException =
      RpxnowException String
    | NormalizationException String
    | DiscoveryException String
    | AuthenticationException String
  deriving (Show, Typeable)
instance Exception AuthenticateException
