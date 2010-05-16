{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Yesod.Dispatch
    ( -- * Quasi-quoted routing
      parseRoutes
    , mkYesod
    , mkYesodSub
      -- ** More fine-grained
    , mkYesodData
    , mkYesodDispatch
      -- * Convert to WAI
    , toWaiApp
    , basicHandler
      -- * Utilities
    , fullRender
#if TEST
    , testSuite
#endif
    ) where

import Yesod.Handler
import Yesod.Yesod
import Yesod.Request
import Yesod.Internal

import Web.Routes.Quasi
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W
import qualified Network.Wai.Enumerator as W
import Network.Wai.Middleware.CleanPath
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.Gzip

import qualified Network.Wai.Handler.SimpleServer as SS
import qualified Network.Wai.Handler.CGI as CGI
import System.Environment (getEnvironment)

import qualified Data.ByteString.Char8 as B
import Web.Encodings
import Web.Routes (encodePathInfo)

import Control.Concurrent.MVar
import Control.Arrow ((***))
import Data.Convertible.Text (cs)

import Data.Time

import Control.Monad
import Data.Maybe
import Web.ClientSession

import Data.Serialize

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import System.IO.Unsafe
import Yesod.Content hiding (testSuite)
#else
import Yesod.Content
#endif

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, *not* subsites. See 'mkYesodSub' for the latter.
-- Use 'parseRoutes' to create the 'Resource's.
mkYesod :: String -- ^ name of the argument datatype
        -> [Resource]
        -> Q [Dec]
mkYesod name = fmap (\(x, y) -> x ++ y) . mkYesodGeneral name [] False

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating subsites, *not* sites. See 'mkYesod' for the latter.
-- Use 'parseRoutes' to create the 'Resource's. In general, a subsite is not
-- executable by itself, but instead provides functionality to
-- be embedded in other sites.
mkYesodSub :: String -- ^ name of the argument datatype
           -> [Name] -- ^ a list of classes the master datatype must be an instance of
           -> [Resource]
           -> Q [Dec]
mkYesodSub name clazzes =
    fmap (\(x, y) -> x ++ y) . mkYesodGeneral name clazzes True

-- | Sometimes, you will want to declare your routes in one file and define
-- your handlers elsewhere. For example, this is the only way to break up a
-- monolithic file into smaller parts. This function, paired with
-- 'mkYesodDispatch', do just that.
mkYesodData :: String -> [Resource] -> Q [Dec]
mkYesodData name res = do
    (x, _) <- mkYesodGeneral name [] False res
    let rname = mkName $ "resources" ++ name
    eres <- liftResources res
    let y = [ SigD rname $ ListT `AppT` ConT ''Resource
            , FunD rname [Clause [] (NormalB eres) []]
            ]
    return $ x ++ y

-- | See 'mkYesodData'.
mkYesodDispatch :: String -> [Resource] -> Q [Dec]
mkYesodDispatch name = fmap snd . mkYesodGeneral name [] False

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

mkYesodGeneral :: String -> [Name] -> Bool -> [Resource] -> Q ([Dec], [Dec])
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
    return ([w, x], (if isSub then id else (:) yes) [y, z])

sessionName :: String
sessionName = "_SESSION"

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. You can use 'basicHandler' if you wish.
toWaiApp :: (Yesod y, YesodSite y) => y -> IO W.Application
toWaiApp a =
    return $ gzip
           $ jsonp
           $ cleanPath
           $ toWaiApp' a

toWaiApp' :: (Yesod y, YesodSite y)
          => y
          -> [String]
          -> W.Request
          -> IO W.Response
toWaiApp' y segments env = do
    key' <- encryptKey y
    now <- getCurrentTime
    let getExpires m = fromIntegral (m * 60) `addUTCTime` now
    let exp' = getExpires $ clientSessionDuration y
    let host = W.remoteHost env
    let session' = fromMaybe [] $ do
            raw <- lookup W.Cookie $ W.requestHeaders env
            val <- lookup (B.pack sessionName) $ parseCookies raw
            decodeSession key' now host val
    let site = getSite
        method = B.unpack $ W.methodToBS $ W.requestMethod env
        types = httpAccept env
        pathSegments = filter (not . null) segments
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
    let sessionVal = encodeSession key' exp' host sessionFinal
    let hs' = AddCookie (clientSessionDuration y) sessionName sessionVal
            : hs
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

encodeSession :: Key
              -> UTCTime -- ^ expire time
              -> B.ByteString -- ^ remote host
              -> [(String, String)] -- ^ session
              -> String -- ^ cookie value
encodeSession key expire rhost session' =
    encrypt key $ cs $ encode $ SessionCookie expire rhost session'

decodeSession :: Key
              -> UTCTime -- ^ current time
              -> B.ByteString -- ^ remote host field
              -> B.ByteString -- ^ cookie value
              -> Maybe [(String, String)]
decodeSession key now rhost encrypted = do
    decrypted <- decrypt key $ B.unpack encrypted
    SessionCookie expire rhost' session' <-
        either (const Nothing) Just $ decode
      $ cs decrypted
    guard $ expire > now
    guard $ rhost' == rhost
    return session'

data SessionCookie = SessionCookie UTCTime B.ByteString [(String, String)]
    deriving (Show, Read)
instance Serialize SessionCookie where
    put (SessionCookie a b c) = putTime a >> put b >> put c
    get = do
        a <- getTime
        b <- get
        c <- get
        return $ SessionCookie a b c

putTime :: Putter UTCTime
putTime (UTCTime d t) = do
    put $ toModifiedJulianDay d
    put $ fromEnum t

getTime :: Get UTCTime
getTime = do
    d <- get
    t <- get
    return $ UTCTime (ModifiedJulianDay d) (toEnum t)

#if TEST

testSuite :: Test
testSuite = testGroup "Yesod.Dispatch"
    [ testProperty "encode/decode session" propEncDecSession
    ]

propEncDecSession :: [(String, String)] -> Bool
propEncDecSession session' = unsafePerformIO $ do
    key <- getDefaultKey
    now <- getCurrentTime
    let expire = addUTCTime 1 now
    let rhost = B.pack "some host"
    val <- encodeSession key expire rhost session'
    return $ Just session' ==
             decodeSession key now rhost (B.pack val)

#endif
