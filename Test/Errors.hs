{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
module Test.Errors (testSuite) where

import Yesod
import Yesod.Helpers.Auth
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

data Errors
instance Yesod Errors where
    resources = [$mkResources|
/denied:
    Get: denied
/needs-ident:
    Get: needsIdent
/has-args:
    Get: hasArgs
|]
instance YesodApproot Errors where
    approot _ = "IGNORED/"
instance YesodAuth Errors

denied :: Handler Errors ()
denied = permissionDenied

needsIdent :: Handler Errors (Html, HtmlObject)
needsIdent = do
    i <- authIdentifier
    return (cs "", cs i)

hasArgs :: Handler Errors (Html, HtmlObject)
hasArgs = do
    {- FIXME wait for new request API
    (a, b) <- runRequest $ (,) <$> getParam "firstParam"
                               <*> getParam "secondParam"
    -}
    let (a, b) = ("foo", "bar")
    return (cs "", cs [a :: String, b])

caseErrorMessages :: Assertion
caseErrorMessages = do return ()
{- FIXME
    app <- toWaiApp Errors
    res <- app $ def { pathInfo = B8.pack "/denied/" }
    assertBool "/denied/" $ "Permission denied" `isInfixOf` show res
    res' <- app $ def { pathInfo = B8.pack "/needs-ident/" }
    assertBool "/needs-ident/" $ "IGNORED/auth/openid/" `isInfixOf` show res'
    -}
    {- FIXME this test is not yet ready
    res3 <- app $ def { pathInfo = "/has-args/" }
    assertBool "/has-args/" $ "secondParam" `isInfixOf` show res3
    -}

testSuite :: Test
testSuite = testGroup "Test.Errors"
    [ testCase "errorMessages" caseErrorMessages
    ]
