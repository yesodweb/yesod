-- this is being re-worked into a general-purpose testing module for Yesod apps
module YesodCoreTest.YesodTest
(   yesod
  , parseRoutes, mkYesod, yesodDispatch, renderRoute, Yesod(..)
  , redirect
  , Approot (..)
  , module Network.Wai
  , module Network.Wai.Test
  , module Test.Hspec
) where

import Yesod.Core
import Network.Wai.Test
import Network.Wai
import Test.Hspec

yesod :: YesodDispatch y => y -> Session a -> IO a
yesod app f = toWaiApp app >>= runSession f
