{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Dispatch
    ( -- * Quasi-quoted routing
      parseRoutes
    , mkYesod
    , mkYesodSub
      -- * Convert to WAI
    , toWaiApp
    , basicHandler
    ) where

import Yesod.Handler
import Yesod.Response
import Yesod.Definitions
import Yesod.Yesod
import Yesod.Request

import Web.Routes.Quasi
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W
import Network.Wai.Middleware.CleanPath
import Network.Wai.Middleware.ClientSession
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.Gzip

import qualified Network.Wai.Handler.SimpleServer as SS
import qualified Network.Wai.Handler.CGI as CGI
import System.Environment (getEnvironment)

import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import Web.Encodings (parseHttpAccept)
import Web.Mime
import Data.List (intercalate)
import Web.Routes (encodePathInfo, decodePathInfo)

mkYesod :: String -> [Resource] -> Q [Dec]
mkYesod name = mkYesodGeneral name [] False

mkYesodSub :: String -> [Name] -> [Resource] -> Q [Dec]
mkYesodSub name clazzes = mkYesodGeneral name clazzes True

explodeHandler :: HasReps c
               => GHandler sub master c
               -> (Routes master -> String)
               -> Routes sub
               -> (Routes sub -> Routes master)
               -> master
               -> (master -> sub)
               -> YesodApp
               -> String
               -> YesodApp
explodeHandler a b c d e f _ _ = runHandler a b (Just c) d e f

mkYesodGeneral :: String -> [Name] -> Bool -> [Resource] -> Q [Dec]
mkYesodGeneral name clazzes isSub res = do
    let name' = mkName name
    let tySyn = TySynInstD ''Routes [ConT name'] (ConT $ mkName $ name ++ "Routes")
    let site = mkName $ "site" ++ name
    let gsbod = NormalB $ VarE site
    let yes' = FunD (mkName "getSite") [Clause [] gsbod []]
    let yes = InstanceD [] (ConT ''YesodSite `AppT` ConT name') [yes']
    explode <- [|explodeHandler|]
    CreateRoutesResult x _ z <- createRoutes CreateRoutesSettings
                { crRoutes = mkName $ name ++ "Routes"
                , crApplication = ConT ''YesodApp
                , crArgument = ConT $ mkName name
                , crExplode = explode
                , crResources = res
                , crSite = site
                }
    let master = if isSub
                    then VarT (mkName "master")
                    else ConT (mkName name)
        murl = ConT ''Routes `AppT` master
        sub = ConT $ mkName name
        surl = ConT ''Routes `AppT` sub
    let yType = ConT ''QuasiSite
                    `AppT` ConT ''YesodApp
                    `AppT` surl
                    `AppT` sub
                    `AppT` murl
                    `AppT` master
    let ctx = if isSub
                then map (flip ClassP [master]) clazzes
                else []
        tvs = [PlainTV $ mkName "master" | isSub]
    let y' = SigD site $ ForallT tvs ctx yType
    return $ (if isSub then id else (:) yes) [y', z, tySyn, x]

toWaiApp :: Yesod y => y -> IO W.Application
toWaiApp a = do
    key' <- encryptKey a
    let mins = clientSessionDuration a
    return $ gzip
           $ jsonp
           $ methodOverride
           $ cleanPath
           $ \thePath -> clientsession encryptedCookies key' mins
           $ toWaiApp' a thePath

toWaiApp' :: Yesod y
          => y
          -> [B.ByteString]
          -> [(B.ByteString, B.ByteString)]
          -> W.Request
          -> IO W.Response
toWaiApp' y resource session env = do
    let site = getSite
        method = B.unpack $ W.methodToBS $ W.requestMethod env
        types = httpAccept env
        pathSegments = filter (not . null) $ cleanupSegments resource
        eurl = quasiParse site pathSegments
        render u = approot y ++ '/'
                 : encodePathInfo (fixSegs $ quasiRender site u)
    rr <- parseWaiRequest env session
    onRequest y rr
    print pathSegments -- FIXME remove
    let ya = case eurl of
                Nothing -> runHandler (errorHandler y NotFound)
                                      render
                                      Nothing
                                      id
                                      y
                                      id
                Just url -> quasiDispatch site
                                render
                                url
                                id
                                y
                                id
                                (badMethodApp method)
                                method
    let eh er = runHandler (errorHandler y er) render eurl id y id
    unYesodApp ya eh rr types >>= responseToWaiResponse

cleanupSegments :: [B.ByteString] -> [String]
cleanupSegments = decodePathInfo . intercalate "/" . map B.unpack

httpAccept :: W.Request -> [ContentType]
httpAccept = map (contentTypeFromString . B.unpack)
           . parseHttpAccept
           . fromMaybe B.empty
           . lookup W.Accept
           . W.requestHeaders

-- | Runs an application with CGI if CGI variables are present (namely
-- PATH_INFO); otherwise uses SimpleServer.
basicHandler :: Int -- ^ port number
             -> W.Application -> IO ()
basicHandler port app = do
    vars <- getEnvironment
    case lookup "PATH_INFO" vars of
        Nothing -> do
            putStrLn $ "http://localhost:" ++ show port ++ "/"
            SS.run port app
        Just _ -> CGI.run app

badMethodApp :: String -> YesodApp
badMethodApp m = YesodApp $ \eh req cts
         -> unYesodApp (eh $ BadMethod m) eh req cts

fixSegs :: [String] -> [String]
fixSegs [] = []
fixSegs [x]
    | any (== '.') x = [x]
    | otherwise = [x, ""] -- append trailing slash
fixSegs (x:xs) = x : fixSegs xs
