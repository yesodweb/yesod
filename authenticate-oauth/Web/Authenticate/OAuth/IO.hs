{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- | This module is deprecated due to the interface change at conduit-0.3.
--   For now, this package only re-exports 'Web.Authenticate.OAuth' module.
module Web.Authenticate.OAuth.IO
    {-# DEPRECATED "This module is deprecated; use Web.Authenticate.OAuth instead." #-}
    ( 
      module Web.Authenticate.OAuth,
    ) where
import Web.Authenticate.OAuth
