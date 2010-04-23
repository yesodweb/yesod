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
import Web.Encodings
import Web.Mime
import Data.List (intercalate)
import Web.Routes (encodePathInfo, decodePathInfo)

import Control.Concurrent.MVar
import Control.Arrow ((***))
import Data.Convertible.Text (cs)

import Data.Time.Clock

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, *not* subsites. See 'mkYesodSub' for the latter.
-- Use 'parseRoutes' in generate to create the 'Resource's.
mkYesod :: String -- ^ name of the argument datatype
        -> [Resource]
        -> Q [Dec]
mkYesod name = mkYesodGeneral name [] False

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating subsites, *not* sites. See 'mkYesod' for the latter.
-- Use 'parseRoutes' in generate to create the 'Resource's. In general, a
-- subsite is not executable by itself, but instead provides functionality to
-- be embedded in other sites.
mkYesodSub :: String -- ^ name of the argument datatype
           -> [Name] -- ^ a list of classes the master datatype must be an instance of
           -> [Resource]
           -> Q [Dec]
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
    let site = mkName $ "site" ++ name
    let gsbod = NormalB $ VarE site
    let yes' = FunD (mkName "getSite") [Clause [] gsbod []]
    let yes = InstanceD [] (ConT ''YesodSite `AppT` ConT name') [yes']
    explode <- [|explodeHandler|]
    QuasiSiteDecs w x y z <- createQuasiSite QuasiSiteSettings
                { crRoutes = mkName $ name ++ "Routes"
                , crApplication = ConT ''YesodApp
                , crArgument = ConT $ mkName name
                , crExplode = explode
                , crResources = res
                , crSite = site
                , crMaster = if isSub then Right clazzes else Left (ConT name')
                }
    return $ (if isSub then id else (:) yes) [w, x, y, z]

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. You can use 'basicHandler' if you wish.
toWaiApp :: Yesod y => y -> IO W.Application
toWaiApp a = do
    key' <- encryptKey a
    let mins = clientSessionDuration a
    return $ gzip
           $ jsonp
           $ methodOverride
           $ cleanPath
           $ \thePath -> clientsession encryptedCookies key' mins -- FIXME allow user input for encryptedCookies
           $ toWaiApp' a thePath

toWaiApp' :: Yesod y
          => y
          -> [B.ByteString]
          -> [(B.ByteString, B.ByteString)]
          -> W.Request
          -> IO W.Response
toWaiApp' y resource session' env = do
    let site = getSite
        method = B.unpack $ W.methodToBS $ W.requestMethod env
        types = httpAccept env
        pathSegments = filter (not . null) $ cleanupSegments resource
        eurl = quasiParse site pathSegments
        render u = approot y ++ '/'
                 : encodePathInfo (fixSegs $ quasiRender site u)
    rr <- parseWaiRequest env session'
    onRequest y rr
    print pathSegments -- FIXME remove
    let ya = case eurl of
                Left _ -> runHandler (errorHandler y NotFound)
                                      render
                                      Nothing
                                      id
                                      y
                                      id
                Right url -> quasiDispatch site
                                render
                                url
                                id
                                y
                                id
                                (badMethodApp method)
                                method
    let eurl' = either (const Nothing) Just eurl
    let eh er = runHandler (errorHandler y er) render eurl' id y id
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

parseWaiRequest :: W.Request
                -> [(B.ByteString, B.ByteString)] -- ^ session
                -> IO Request
parseWaiRequest env session' = do
    let gets' = map (cs *** cs) $ decodeUrlPairs $ W.queryString env
    let reqCookie = fromMaybe B.empty $ lookup W.Cookie $ W.requestHeaders env
        cookies' = map (cs *** cs) $ parseCookies reqCookie
        acceptLang = lookup W.AcceptLanguage $ W.requestHeaders env
        langs = map cs $ maybe [] parseHttpAccept acceptLang
        langs' = case lookup langKey cookies' of
                    Nothing -> langs
                    Just x -> x : langs
        langs'' = case lookup langKey gets' of
                     Nothing -> langs'
                     Just x -> x : langs'
        session'' = map (cs *** cs) session'
    rbthunk <- iothunk $ rbHelper env
    return $ Request gets' cookies' session'' rbthunk env langs''

rbHelper :: W.Request -> IO RequestBodyContents
rbHelper = fmap (fix1 *** map fix2) . parseRequestBody lbsSink where
    fix1 = map (cs *** cs)
    fix2 (x, FileInfo a b c) = (cs x, FileInfo (cs a) (cs b) c)

-- | Produces a \"compute on demand\" value. The computation will be run once
-- it is requested, and then the result will be stored. This will happen only
-- once.
iothunk :: IO a -> IO (IO a)
iothunk = fmap go . newMVar . Left where
    go :: MVar (Either (IO a) a) -> IO a
    go mvar = modifyMVar mvar go'
    go' :: Either (IO a) a -> IO (Either (IO a) a, a)
    go' (Right val) = return (Right val, val)
    go' (Left comp) = do
        val <- comp
        return (Right val, val)

responseToWaiResponse :: (W.Status, [Header], ContentType, Content)
                      -> IO W.Response
responseToWaiResponse (sc, hs, ct, c) = do
    hs' <- mapM headerToPair hs
    let hs'' = (W.ContentType, cs $ contentTypeToString ct) : hs'
    return $ W.Response sc hs'' $ case c of
                                    ContentFile fp -> Left fp
                                    ContentEnum e -> Right $ W.Enumerator e

-- | Convert Header to a key/value pair.
headerToPair :: Header -> IO (W.ResponseHeader, B.ByteString)
headerToPair (AddCookie minutes key value) = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral $ minutes * 60) now
    return (W.SetCookie, cs $ key ++ "=" ++ value ++"; path=/; expires="
                              ++ formatW3 expires)
headerToPair (DeleteCookie key) = return
    (W.SetCookie, cs $
     key ++ "=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT")
headerToPair (Header key value) =
    return (W.responseHeaderFromBS $ cs key, cs value)
