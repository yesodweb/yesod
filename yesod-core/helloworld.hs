{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
import Yesod.Core
import Network.Wai.Handler.Warp (run)
import Data.Text (unpack)

data Subsite = Subsite String

mkYesodSub "Subsite" [] [$parseRoutes|
/ SubRootR GET
/multi/*Strings SubMultiR
|]

getSubRootR :: Yesod m => GHandler Subsite m RepPlain
getSubRootR = do
    Subsite s <- getYesodSub
    tm <- getRouteToMaster
    render <- getUrlRender
    $(logDebug) "I'm in SubRootR"
    return $ RepPlain $ toContent $ "Hello Sub World: " ++ s ++ ". " ++ unpack (render (tm SubRootR))

handleSubMultiR :: Yesod m => Strings -> GHandler Subsite m RepPlain
handleSubMultiR x = do
    Subsite y <- getYesodSub
    $(logInfo) "In SubMultiR"
    return . RepPlain . toContent . show $ (x, y)

data HelloWorld = HelloWorld { getSubsite :: String -> Subsite }
mkYesod "HelloWorld" [$parseRoutes|
/ RootR GET
/subsite/#String SubsiteR Subsite getSubsite
|]
instance Yesod HelloWorld where approot _ = ""
-- getRootR :: GHandler HelloWorld HelloWorld RepPlain -- FIXME remove type sig
getRootR = do
    $(logOther "HAHAHA") "Here I am"
    return $ RepPlain "Hello World"
main = toWaiApp (HelloWorld Subsite) >>= run 3000
