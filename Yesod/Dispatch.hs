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
      -- * Utilities
    , fullRender
    ) where

import Yesod.Handler
import Yesod.Content
import Yesod.Yesod
import Yesod.Request
import Yesod.Internal

import Web.Routes.Quasi
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W
import qualified Network.Wai.Enumerator as W
import Network.Wai.Middleware.CleanPath
import Network.Wai.Middleware.ClientSession
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.Gzip

import qualified Network.Wai.Handler.SimpleServer as SS
import qualified Network.Wai.Handler.CGI as CGI
import System.Environment (getEnvironment)

import qualified Data.ByteString.Char8 as B
import Web.Encodings
import Data.List (intercalate)
import Web.Routes (encodePathInfo, decodePathInfo)

import Control.Concurrent.MVar
import Control.Arrow ((***))
import Data.Convertible.Text (cs)

import Data.Time

import Control.Monad
import Data.Maybe
import Web.ClientSession

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, *not* subsites. See 'mkYesodSub' for the latter.
-- Use 'parseRoutes' to create the 'Resource's.
mkYesod :: String -- ^ name of the argument datatype
        -> [Resource]
        -> Q [Dec]
mkYesod name = mkYesodGeneral name [] False

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating subsites, *not* sites. See 'mkYesod' for the latter.
-- Use 'parseRoutes' to create the 'Resource's. In general, a subsite is not
-- executable by itself, but instead provides functionality to
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

sessionName :: String
sessionName = "_SESSION"

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. You can use 'basicHandler' if you wish.
toWaiApp :: Yesod y => y -> IO W.Application
toWaiApp a =
    return $ gzip
           $ jsonp
           $ methodOverride
           $ cleanPath
           $ toWaiApp' a

parseSession :: B.ByteString -> [(String, String)]
parseSession bs = case reads $ cs bs of
                    [] -> []
                    ((x, _):_) -> x

toWaiApp' :: Yesod y
          => y
          -> [B.ByteString]
          -> W.Request
          -> IO W.Response
toWaiApp' y resource env = do
    key' <- encryptKey y
    now <- getCurrentTime
    let getExpires m = fromIntegral (m * 60) `addUTCTime` now
    let exp' = getExpires $ clientSessionDuration y
    let host = W.remoteHost env
    let session' = do
            (_, raw) <- filter (\(x, _) -> x == W.Cookie) $ W.requestHeaders env
            (name, val) <- parseCookies raw
            guard $ name == B.pack sessionName
            decoded <- maybeToList $ decodeCookie key' now host val
            parseSession decoded
        site = getSite
        method = B.unpack $ W.methodToBS $ W.requestMethod env
        types = httpAccept env
        pathSegments = filter (not . null) $ cleanupSegments resource
        eurl = quasiParse site pathSegments
        render u = fromMaybe
                    (fullRender (approot y) site u)
                    (urlRenderOverride y u)
    rr <- parseWaiRequest env session'
    onRequest y rr
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
    (s, hs, ct, c, sessionFinal) <- unYesodApp ya eh rr types
    let sessionVal = encrypt key' $ B.pack $ show $ ACookie exp' host $ B.pack
                                  $ show sessionFinal
    let hs' = AddCookie (clientSessionDuration y) sessionName sessionVal : hs
        hs'' = map (headerToPair getExpires) hs'
        hs''' = (W.ContentType, cs $ contentTypeToString ct) : hs''
    return $ W.Response s hs''' $ case c of
                                    ContentFile fp -> Left fp
                                    ContentEnum e -> Right $ W.buffer
                                                           $ W.Enumerator e

-- | Fully render a route to an absolute URL. Since Yesod does this for you
-- internally, you will rarely need access to this. However, if you need to
-- generate links *outside* of the Handler monad, this may be useful.
--
-- For example, if you want to generate an e-mail which links to your site,
-- this is the function you would want to use.
fullRender :: String -- ^ approot, no trailing slash
           -> QuasiSite YesodApp arg arg
           -> Routes arg
           -> String
fullRender ar site route =
    ar ++ '/' : encodePathInfo (fixSegs $ quasiRender site route)

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
                -> [(String, String)] -- ^ session
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
    rbthunk <- iothunk $ rbHelper env
    return $ Request gets' cookies' session' rbthunk env langs''

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

-- | Convert Header to a key/value pair.
headerToPair :: (Int -> UTCTime) -- ^ minutes -> expiration time
             -> Header
             -> (W.ResponseHeader, B.ByteString)
headerToPair getExpires (AddCookie minutes key value) =
    let expires = getExpires minutes
     in (W.SetCookie, cs $ key ++ "=" ++ value ++"; path=/; expires="
                              ++ formatW3 expires)
headerToPair _ (DeleteCookie key) =
    (W.SetCookie, cs $
     key ++ "=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT")
headerToPair _ (Header key value) = (W.responseHeaderFromBS $ cs key, cs value)

decodeCookie :: Word256 -- ^ key
             -> UTCTime -- ^ current time
             -> B.ByteString -- ^ remote host field
             -> B.ByteString -- ^ cookie value
             -> Maybe B.ByteString
decodeCookie key now rhost encrypted = do
    decrypted <- decrypt key $ B.unpack encrypted
    (ACookie expire rhost' val) <-
        case reads $ B.unpack decrypted of
            [] -> Nothing
            ((x, _):_) -> Just x
    guard $ expire > now
    guard $ rhost' == rhost
    guard $ not $ B.null val
    return val

data ACookie = ACookie UTCTime B.ByteString B.ByteString
    deriving (Show, Read)
