{-# LANGUAGE DeriveDataTypeable #-}
module Web.Authenticate.Internal
    ( AuthenticateException (..)
    ) where

import Data.Typeable (Typeable)
import Control.Exception (Exception)

data AuthenticateException =
      RpxnowException String
    | NormalizationException String
    | DiscoveryException String
    | AuthenticationException String
  deriving (Show, Typeable)
instance Exception AuthenticateException
