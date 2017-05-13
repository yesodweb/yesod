{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.Exceptions
    ( exceptionsTest
    , Widget
    , resourcesY
    ) where

import Test.Hspec

import Yesod.Core
import Yesod.Core.Types (HandlerContents (HCError))
import Control.Exception (throwIO)
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types (status301)

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/redirect RedirR GET
/impure ImpureR GET
|]

instance Yesod Y where
    approot = ApprootStatic "http://test"
    errorHandler (InternalError e) = do
        _ <- return $! e
        addHeader "ERROR" "HANDLER"
        return $ toTypedContent e
    errorHandler x = defaultErrorHandler x

getRootR :: Handler ()
getRootR = error "FOOBAR" >> return ()

getRedirR :: Handler ()
getRedirR = do
    addHeader "foo" "bar"
    redirectWith status301 RootR

getImpureR :: Handler ()
getImpureR = liftIO $ throwIO $ HCError $ InternalError $ error "impure!"

exceptionsTest :: Spec
exceptionsTest = describe "Test.Exceptions" $ do
      it "500" case500
      it "redirect keeps headers" caseRedirect
      it "deals with impure InternalError values" caseImpure

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

case500 :: IO ()
case500 = runner $ do
    res <- request defaultRequest
    assertStatus 500 res
    assertBodyContains "FOOBAR" res

caseRedirect :: IO ()
caseRedirect = runner $ do
    res <- request defaultRequest { pathInfo = ["redirect"] }
    assertStatus 301 res
    assertHeader "foo" "bar" res

caseImpure :: IO ()
caseImpure = runner $ do
    res <- request defaultRequest { pathInfo = ["impure"] }
    assertStatus 500 res
    assertBodyContains "impure!" res
    assertHeader "ERROR" "HANDLER" res
