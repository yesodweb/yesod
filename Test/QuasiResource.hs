{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.QuasiResource (testSuite) where

import Yesod
import Text.StringTemplate (nullGroup)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.List

data MyYesod = MyYesod

instance Show (Handler MyYesod ChooseRep) where show _ = "Another handler"

getStatic :: Verb -> [String] -> Handler MyYesod HtmlObject
getStatic v p = return $ toHtmlObject ["getStatic", show v, show p]
pageIndex :: Handler MyYesod HtmlObject
pageIndex = return $ toHtmlObject ["pageIndex"]
pageAdd :: Handler MyYesod ChooseRep
pageAdd = return $ chooseRep $ toHtmlObject ["pageAdd"]
pageDetail :: String -> Handler MyYesod ChooseRep
pageDetail s = return $ chooseRep $ toHtmlObject ["pageDetail", s]
pageDelete :: String -> Handler MyYesod HtmlObject
pageDelete s = return $ toHtmlObject ["pageDelete", s]
pageUpdate :: String -> Handler MyYesod ChooseRep
pageUpdate s = return $ chooseRep $ toHtmlObject ["pageUpdate", s]
userInfo :: Int -> Handler MyYesod HtmlObject
userInfo i = return $ toHtmlObject ["userInfo", show i]
userVariable :: Int -> String -> Handler MyYesod HtmlObject
userVariable i s = return $ toHtmlObject ["userVariable", show i, s]
userPage :: Int -> [String] -> Handler MyYesod HtmlObject
userPage i p = return $ toHtmlObject ["userPage", show i, show p]

instance Show (Verb -> Handler MyYesod ChooseRep) where
    show _ = "verb -> handler"
instance Show (Resource -> Verb -> Handler MyYesod ChooseRep) where
    show _ = "resource -> verb -> handler"
handler :: Resource -> Verb -> Handler MyYesod ChooseRep
handler = [$resources|
/static/*filepath/: getStatic
/page/:
    Get: pageIndex
    Put: pageAdd
/page/$page/:
    Get: pageDetail
    Delete: pageDelete
    Post: pageUpdate
/user/#id/:
    Get: userInfo
/user/#id/profile/$variable/:
    Get: userVariable
/user/#id/page/*page/:
    Get: userPage
|]

ph :: [String] -> Handler MyYesod ChooseRep -> Assertion
ph ss h = do
    let eh = return . chooseRep . toHtmlObject . show
        rr = error "No raw request"
        y = MyYesod
        cts = [TypeHtml]
    res <- runHandler h eh rr y nullGroup cts
    res' <- myShow res
    mapM_ (helper res') ss
      where
        helper haystack needle =
            assertBool needle $ needle `isInfixOf` haystack

myShow :: Response -> IO String
myShow (Response sc hs ct c) = runContent c >>= \c' -> return $ unlines
    [ show sc
    , unlines $ map show hs
    , show ct
    , show c'
    ]

caseQuasi :: Assertion
caseQuasi = do
    ph ["200", "foo"] $ handler ["static", "foo", "bar", "baz"] Get
    ph ["404"] $ handler ["foo", "bar", "baz"] Get
    ph ["200", "pageIndex"] $ handler ["page"] Get
    ph ["404"] $ handler ["user"] Get
    ph ["404"] $ handler ["user", "five"] Get
    ph ["200", "userInfo", "5"] $ handler ["user", "5"] Get
    ph ["200", "userVar"] $ handler ["user", "5", "profile", "email"] Get

testSuite :: Test
testSuite = testGroup "Test.QuasiResource"
    [ testCase "quasi" caseQuasi
    ]
