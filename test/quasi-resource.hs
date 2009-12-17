{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Yesod.Rep
import Data.Object.Html

data MyYesod = MyYesod

instance Show (Handler MyYesod RepChooser) where show _ = "Another handler"

getStatic :: Verb -> Handler MyYesod RepChooser
getStatic v = return $ chooseRep $ toHtmlObject ["getStatic", show v]
pageIndex :: Handler MyYesod RepChooser
pageIndex = return $ chooseRep $ toHtmlObject ["pageIndex"]
pageAdd :: Handler MyYesod RepChooser
pageAdd = return $ chooseRep $ toHtmlObject ["pageAdd"]
pageDetail :: Handler MyYesod RepChooser
pageDetail = return $ chooseRep $ toHtmlObject ["pageDetail"]
pageDelete :: Handler MyYesod RepChooser
pageDelete = return $ chooseRep $ toHtmlObject ["pageDelete"]
pageUpdate :: Handler MyYesod RepChooser
pageUpdate = return $ chooseRep $ toHtmlObject ["pageUpdate"]
userInfo :: Handler MyYesod RepChooser
userInfo = return $ chooseRep $ toHtmlObject ["userInfo"]

instance Show (Verb -> Handler MyYesod RepChooser) where
    show _ = "verb -> handler"
instance Show (Resource -> Verb -> Handler MyYesod RepChooser) where
    show _ = "resource -> verb -> handler"
handler :: Resource -> Verb -> Handler MyYesod RepChooser
handler = [$rpnodesQuasi|
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
|]

ph :: Handler MyYesod RepChooser -> IO ()
ph h = do
    let eh e = return $ chooseRep $ toHtmlObject $ show e
        rr = error "No raw request"
        y = MyYesod
        cts = [TypeHtml]
    res <- runHandler h eh rr y cts
    print res

main :: IO ()
main = do
    ph $ handler ["static", "foo", "bar", "baz"] Get
    ph $ handler ["foo", "bar", "baz"] Get
    ph $ handler ["page"] Get
    ph $ handler ["user"] Get
    ph $ handler ["user", "five"] Get
    ph $ handler ["user", "5"] Get
