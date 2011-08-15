{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.NoOverloadedStrings (noOverloadedTest) where

import Test.Hspec
import Test.Hspec.HUnit

import Yesod.Core hiding (Request)
import Network.Wai.Test
import Network.Wai
import Data.Monoid (mempty)
import Data.String (fromString)

data Subsite = Subsite
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

instance Yesod Y where
    approot _ = fromString ""

getRootR = return ()
getFooR = return ()

runner f = toWaiApp Y >>= runSession f
defaultRequest = Request
    { pathInfo = []
    , requestHeaders = []
    , queryString = []
    , requestMethod = fromString "GET"
    }

case_sanity = runner $ do
    res <- request defaultRequest
    assertBody mempty res

noOverloadedTest :: IO [IO Spec]
noOverloadedTest = describe "Test.NoOverloadedStrings"
    [ it "sanity" case_sanity
    ]
