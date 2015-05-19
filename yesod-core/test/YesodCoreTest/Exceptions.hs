{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.Exceptions (exceptionsTest) where

import Test.Hspec

import Yesod.Core
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types (status301)

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/redirect RedirR GET
|]

instance Yesod Y where
    approot = ApprootStatic "http://test"
    errorHandler (InternalError e) = return $ toTypedContent e
    errorHandler x = defaultErrorHandler x

getRootR :: Handler ()
getRootR = error "FOOBAR" >> return ()

getRedirR :: Handler ()
getRedirR = do
    addHeader "foo" "bar"
    redirectWith status301 RootR

exceptionsTest :: Spec
exceptionsTest = describe "Test.Exceptions" $ do
      it "500" case500
      it "redirect keeps headers" caseRedirect

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

case500 :: IO ()
case500 = runner $ do
    res <- request defaultRequest
    assertStatus 500 res
    assertBody "FOOBAR" res

caseRedirect :: IO ()
caseRedirect = runner $ do
    res <- request defaultRequest { pathInfo = ["redirect"] }
    assertStatus 301 res
    assertHeader "foo" "bar" res
