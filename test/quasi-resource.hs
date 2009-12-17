{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

import Yesod
import Yesod.Rep

data MyYesod = MyYesod

instance Show (Handler MyYesod RepChooser) where show _ = "Another handler"

getStatic :: Handler MyYesod RepChooser
getStatic = undefined
pageIndex :: Handler MyYesod RepChooser
pageIndex = undefined
pageAdd :: Handler MyYesod RepChooser
pageAdd = undefined
pageDetail :: Handler MyYesod RepChooser
pageDetail = undefined
pageDelete :: Handler MyYesod RepChooser
pageDelete = undefined
pageUpdate :: Handler MyYesod RepChooser
pageUpdate = undefined
userInfo :: Handler MyYesod RepChooser
userInfo = undefined

instance Show (Verb -> Handler MyYesod RepChooser) where
    show _ = "verb -> handler"
handler :: [(RP, Verb -> Handler MyYesod RepChooser)]
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

main :: IO ()
main = do
    print handler
