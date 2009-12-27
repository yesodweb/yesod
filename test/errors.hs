{-# LANGUAGE QuasiQuotes #-}
import Yesod
import Hack
import Data.Default
import Data.List

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

denied :: Handler Errors ()
denied = permissionDenied

needsIdent :: Handler Errors HtmlObject
needsIdent = do
    i <- identifier
    return $ toHtmlObject i

hasArgs :: Handler Errors HtmlObject
hasArgs = do
    -- FIXME this test needs more work
    a <- getParam "firstParam"
    b <- getParam "secondParam"
    return $ toHtmlObject [a :: String, b]

main = do
    let app = toHackApp Errors
    res <- app $ def { pathInfo = "/denied/" }
    print res
    print $ "Permission denied" `isInfixOf` show res
    res' <- app $ def { pathInfo = "/needs-ident/" }
    print res'
    print $ "Permission denied" `isInfixOf` show res'
    res3 <- app $ def { pathInfo = "/has-args/" }
    print res3
    print $ "secondParam" `isInfixOf` show res3
