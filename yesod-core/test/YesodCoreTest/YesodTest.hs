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

import Yesod.Core hiding (Request)
import Network.Wai.Test
import Network.Wai
import Test.Hspec
import Test.Hspec.HUnit()

yesod :: (YesodDispatch y y, Yesod y) => y -> Session a -> IO a
yesod app f = toWaiApp app >>= runSession f
