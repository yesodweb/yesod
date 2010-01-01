{-# LANGUAGE QuasiQuotes #-}
module Test.Errors (testSuite) where

import Yesod
import Yesod.Helpers.Auth
import Hack
import Data.Default
import Data.List
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Applicative

data Errors = Errors
instance Yesod Errors where
    handlers = [$resources|
/denied:
    Get: denied
/needs-ident:
    Get: needsIdent
/has-args:
    Get: hasArgs
|]
instance YesodApproot Errors where
    approot _ = Approot "IGNORED/"
instance YesodAuth Errors

denied :: Handler Errors ()
denied = permissionDenied

needsIdent :: Handler Errors HtmlObject
needsIdent = do
    i <- authIdentifier
    return $ toHtmlObject i

hasArgs :: Handler Errors HtmlObject
hasArgs = do
    (a, b) <- runRequest $ (,) <$> getParam "firstParam"
                               <*> getParam "secondParam"
    return $ toHtmlObject [a :: String, b]

caseErrorMessages :: Assertion
caseErrorMessages = do
    let app = toHackApp Errors
    res <- app $ def { pathInfo = "/denied/" }
    assertBool "/denied/" $ "Permission denied" `isInfixOf` show res
    res' <- app $ def { pathInfo = "/needs-ident/" }
    assertBool "/needs-ident/" $ "IGNORED/auth/openid/" `isInfixOf` show res'
    res3 <- app $ def { pathInfo = "/has-args/" }
    assertBool "/has-args/" $ "secondParam" `isInfixOf` show res3

testSuite :: Test
testSuite = testGroup "Test.Errors"
    [ testCase "errorMessages" caseErrorMessages
    ]
