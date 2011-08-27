{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Exceptions (exceptionsTest) where

import Test.Hspec
import Test.Hspec.HUnit

import Yesod.Core hiding (Request)
import Yesod.Content
import Yesod.Dispatch
import Yesod.Handler (Route, ErrorResponse (InternalError))

import Network.Wai
import Network.Wai.Test

import qualified Data.ByteString.Lazy.Char8 as L8

data Y = Y
mkYesod "Y" [$parseRoutes|
/ RootR GET
|]

instance Yesod Y where
    approot _ = "http://test"
    errorHandler (InternalError e) = return $ chooseRep $ RepPlain $ toContent e
    errorHandler x = defaultErrorHandler x

getRootR = error "FOOBAR" >> return ()

exceptionsTest :: IO [IO Spec]
exceptionsTest = describe "Test.Exceptions"
    [ it "500" case500
    ]

runner f = toWaiApp Y >>= runSession f

case500 = runner $ do
    res <- request defaultRequest
    assertStatus 500 res
    assertBody "FOOBAR" res
