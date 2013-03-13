{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.NoOverloadedStringsSub where

import Yesod.Core
import Network.Wai
import Network.Wai.Test
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

data Subsite = Subsite

mkYesodSubData "Subsite" [parseRoutes|
/bar BarR GET
/baz BazR GET
|]
