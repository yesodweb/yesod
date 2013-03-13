{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.NoOverloadedStrings (noOverloadedTest, Widget) where

import Test.Hspec

import Yesod.Core
import Network.Wai
import Network.Wai.Test
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

data Subsite = Subsite

getSubsite :: a -> Subsite
getSubsite = const Subsite

mkYesodSub "Subsite" [] [parseRoutes|
/bar BarR GET
|]

getBarR :: Monad m => m T.Text
getBarR = return $ T.pack "BarR"

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

case_subsite :: IO ()
case_subsite = runner $ do
    res <- request defaultRequest
        { pathInfo = map T.pack ["subsite", "bar"]
        }
    assertBody (L8.pack "BarR") res
    assertStatus 200 res

noOverloadedTest :: Spec
noOverloadedTest = describe "Test.NoOverloadedStrings" $ do
      it "sanity" case_sanity
      it "subsite" case_subsite
