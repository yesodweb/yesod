{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Text.StringTemplate (nullGroup)

data MyYesod = MyYesod

instance Show (Handler MyYesod RepChooser) where show _ = "Another handler"

getStatic :: Verb -> [String] -> Handler MyYesod HtmlObject
getStatic v p = return $ toHtmlObject ["getStatic", show v, show p]
pageIndex :: Handler MyYesod HtmlObject
pageIndex = return $ toHtmlObject ["pageIndex"]
pageAdd :: Handler MyYesod RepChooser
pageAdd = return $ chooseRep $ toHtmlObject ["pageAdd"]
pageDetail :: String -> Handler MyYesod RepChooser
pageDetail s = return $ chooseRep $ toHtmlObject ["pageDetail", s]
pageDelete :: String -> Handler MyYesod HtmlObject
pageDelete s = return $ toHtmlObject ["pageDelete", s]
pageUpdate :: String -> Handler MyYesod RepChooser
pageUpdate s = return $ chooseRep $ toHtmlObject ["pageUpdate", s]
userInfo :: Int -> Handler MyYesod HtmlObject
userInfo i = return $ toHtmlObject ["userInfo", show i]
userVariable :: Int -> String -> Handler MyYesod HtmlObject
userVariable i s = return $ toHtmlObject ["userVariable", show i, s]
userPage :: Int -> [String] -> Handler MyYesod HtmlObject
userPage i p = return $ toHtmlObject ["userPage", show i, show p]

instance Show (Verb -> Handler MyYesod RepChooser) where
    show _ = "verb -> handler"
instance Show (Resource -> Verb -> Handler MyYesod RepChooser) where
    show _ = "resource -> verb -> handler"
handler :: Resource -> Verb -> Handler MyYesod RepChooser
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

ph :: Handler MyYesod RepChooser -> IO ()
ph h = do
    let eh = return . chooseRep . toHtmlObject . show
        rr = error "No raw request"
        y = MyYesod
        cts = [TypeHtml]
    res <- runHandler h eh rr y nullGroup cts
    print res

main :: IO ()
main = do
    ph $ handler ["static", "foo", "bar", "baz"] Get
    ph $ handler ["foo", "bar", "baz"] Get
    ph $ handler ["page"] Get
    ph $ handler ["user"] Get
    ph $ handler ["user", "five"] Get
    ph $ handler ["user", "5"] Get
    ph $ handler ["user", "5", "profile", "email"] Get
