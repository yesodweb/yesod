{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.Exceptions (exceptionsTest, Widget) where

import Test.Hspec
import Test.Hspec.HUnit ()

import Yesod.Core hiding (Request)
import Network.Wai.Test

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
|]

instance Yesod Y where
    approot _ = "http://test"
    errorHandler (InternalError e) = return $ chooseRep $ RepPlain $ toContent e
    errorHandler x = defaultErrorHandler x

getRootR :: Handler ()
getRootR = error "FOOBAR" >> return ()

exceptionsTest :: [Spec]
exceptionsTest = describe "Test.Exceptions"
    [ it "500" case500
    ]

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

case500 :: IO ()
case500 = runner $ do
    res <- request defaultRequest
    assertStatus 500 res
    assertBody "FOOBAR" res
