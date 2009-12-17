{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Yesod.Resource
import Yesod.Definitions
import Data.Convertible.Text

handler :: [(RP, [(Verb, [Char])])]
handler =
    $(rpnodesTHCheck
         [ RPNode (cs "static/*filepath") $ AllVerbs "getStatic"
         , RPNode (cs "page") $ Verbs [(Get, "pageIndex"), (Put, "pageAdd")]
         , RPNode (cs "page/$page") $ Verbs [ (Get, "pageDetail")
                                            , (Delete, "pageDelete")
                                            , (Post, "pageUpdate")
                                            ]
         , RPNode (cs "user/#id") $ Verbs [(Get, "userInfo")]
         ])

handler2 :: [(RP, [(Verb, [Char])])]
handler2 = [$rpnodesQuasi|
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
    print handler2
    print $ handler == handler2
