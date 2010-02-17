{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.QuasiResource (testSuite) where

import Yesod
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.List
import Network.Wai (Method (..))

data MyYesod = MyYesod

instance Show (Handler MyYesod ChooseRep) where show _ = "Another handler"

addHead' :: HtmlObject -> (Html, HtmlObject)
addHead' x = (cs "", x)

addHead :: Monad m => HtmlObject -> m (Html, HtmlObject)
addHead = return . addHead'

getStatic :: Method -> [String] -> Handler MyYesod (Html, HtmlObject)
getStatic v p = addHead $ toHtmlObject ["getStatic", show v, show p]
pageIndex :: Handler MyYesod (Html, HtmlObject)
pageIndex = addHead $ toHtmlObject ["pageIndex"]
pageAdd :: Handler MyYesod ChooseRep
pageAdd = return $ chooseRep $ addHead' $ toHtmlObject ["pageAdd"]
pageDetail :: String -> Handler MyYesod ChooseRep
pageDetail s = return $ chooseRep $ addHead' $ toHtmlObject ["pageDetail", s]
pageDelete :: String -> Handler MyYesod (Html, HtmlObject)
pageDelete s = addHead $ toHtmlObject ["pageDelete", s]
pageUpdate :: String -> Handler MyYesod ChooseRep
pageUpdate s = return $ chooseRep $ addHead' $ toHtmlObject ["pageUpdate", s]
userInfo :: Integer -> Handler MyYesod (Html, HtmlObject)
userInfo i = addHead $ toHtmlObject ["userInfo", show i]
userVariable :: Integer -> String -> Handler MyYesod (Html, HtmlObject)
userVariable i s = addHead $ toHtmlObject ["userVariable", show i, s]
userPage :: Integer -> [String] -> Handler MyYesod (Html, HtmlObject)
userPage i p = addHead $ toHtmlObject ["userPage", show i, show p]

instance Show (Method -> Handler MyYesod ChooseRep) where
    show _ = "verb -> handler"
instance Show (Resource -> Method -> Handler MyYesod ChooseRep) where
    show _ = "resource -> verb -> handler"
handler :: Resource -> Method -> Handler MyYesod ChooseRep
handler = [$mkResources|
/static/*filepath/: getStatic
/page/:
    GET: pageIndex
    PUT: pageAdd
/page/$page/:
    GET: pageDetail
    DELETE: pageDelete
    POST: pageUpdate
/user/#id/:
    GET: userInfo
/user/#id/profile/$variable/:
    GET: userVariable
/user/#id/page/*page/:
    GET: userPage
|]

ph :: [String] -> Handler MyYesod ChooseRep -> Assertion
ph ss h = do
    let eh = return . chooseRep . addHead' . toHtmlObject . show
        rr = error "No raw request"
        y = MyYesod
        cts = [TypeHtml]
    res <- runHandler h eh rr y cts
    res' <- myShow res
    mapM_ (helper res') ss
      where
        helper haystack needle =
            assertBool (show ("needle", needle, ss, haystack))
                $ needle `isInfixOf` haystack

myShow :: Response -> IO String
myShow (Response sc hs ct c) = runContent c >>= \c' -> return $ unlines
    [ show sc
    , unlines $ map show hs
    , show ct
    , show c'
    ]

caseQuasi :: Assertion
caseQuasi = do
    ph ["200", "foo"] $ handler ["static", "foo", "bar", "baz"] GET
    ph ["404"] $ handler ["foo", "bar", "baz"] GET
    ph ["200", "pageIndex"] $ handler ["page"] GET
    ph ["404"] $ handler ["user"] GET
    ph ["404"] $ handler ["user", "five"] GET
    ph ["200", "userInfo", "5"] $ handler ["user", "5"] GET
    ph ["200", "userVar"] $ handler ["user", "5", "profile", "email"] GET

testSuite :: Test
testSuite = testGroup "Test.QuasiResource"
    [ testCase "quasi" caseQuasi
    ]
