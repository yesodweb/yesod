{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.NoOverloadedStrings (noOverloadedTest, Widget) where

import Test.Hspec

import Yesod.Core hiding (Request)
import Network.Wai.Test
import Data.Monoid (mempty)

data Subsite = Subsite

getSubsite :: a -> Subsite
getSubsite = const Subsite

mkYesodSub "Subsite" [] [parseRoutes|
/bar BarR GET
|]

getBarR :: GHandler Subsite m ()
getBarR = return ()

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/foo FooR GET
/subsite SubsiteR Subsite getSubsite
|]

instance Yesod Y

getRootR :: Handler ()
getRootR = return ()

getFooR :: Handler ()
getFooR = return ()

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

case_sanity :: IO ()
case_sanity = runner $ do
    res <- request defaultRequest
    assertBody mempty res

noOverloadedTest :: Spec
noOverloadedTest = describe "Test.NoOverloadedStrings" $ do
      it "sanity" case_sanity
