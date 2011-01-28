{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
import Yesod.Core
import Yesod.Dispatch
import Yesod.Content
import Yesod.Handler
import Network.Wai.Handler.Warp (runEx)

data Subsite = Subsite String

mkYesodSub "Subsite" [] [$parseRoutes|
/ SubRootR GET
/multi/*Strings SubMultiR
|]

getSubRootR :: GHandler Subsite m RepPlain
getSubRootR = do
    Subsite s <- getYesodSub
    tm <- getRouteToMaster
    render <- getUrlRender
    return $ RepPlain $ toContent $ "Hello Sub World: " ++ s ++ ". " ++ render (tm SubRootR)

handleSubMultiR :: Strings -> GHandler Subsite m RepPlain
handleSubMultiR x = do
    Subsite y <- getYesodSub
    return . RepPlain . toContent . show $ (x, y)

data HelloWorld = HelloWorld { getSubsite :: String -> Subsite }
mkYesod "HelloWorld" [$parseRoutes|
/ RootR GET
/subsite/#String SubsiteR Subsite getSubsite
|]
instance Yesod HelloWorld where approot _ = ""
-- getRootR :: GHandler HelloWorld HelloWorld RepPlain -- FIXME remove type sig
getRootR = return $ RepPlain "Hello World"
main = toWaiApp (HelloWorld Subsite) >>= runEx print 3000
