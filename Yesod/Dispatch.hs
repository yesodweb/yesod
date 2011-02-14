{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Dispatch
    ( -- * Quasi-quoted routing
      parseRoutes
    , mkYesod
    , mkYesodSub
      -- ** More fine-grained
    , mkYesodData
    , mkYesodSubData
    , mkYesodDispatch
    , mkYesodSubDispatch 
      -- ** Path pieces
    , SinglePiece (..)
    , MultiPiece (..)
    , Strings
      -- * Convert to WAI
    , toWaiApp
    , basicHandler
    , basicHandler'
#if TEST
    , testSuite
#endif
    ) where

#if TEST
import Yesod.Yesod hiding (testSuite, Key)
import Yesod.Handler hiding (testSuite)
#else
import Yesod.Yesod hiding (Key)
import Yesod.Handler
#endif

import Yesod.Request
import Yesod.Internal

import Web.Routes.Quasi
import Web.Routes.Quasi.Parse
import Web.Routes.Quasi.TH
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W
import Network.Wai.Middleware.CleanPath (cleanPath)
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.Gzip

import qualified Network.Wai.Handler.SimpleServer as SS
import qualified Network.Wai.Handler.CGI as CGI
import System.Environment (getEnvironment)

import qualified Data.ByteString.Char8 as B

import Control.Concurrent.MVar
import Control.Arrow ((***))

import Data.Time

import Control.Monad
import Data.Maybe
import Web.ClientSession
import qualified Web.ClientSession as CS
import Data.Char (isUpper)

import Data.Serialize
import qualified Data.Serialize as Ser
import Network.Wai.Parse hiding (FileInfo)
import qualified Network.Wai.Parse as NWP
import Data.String (fromString)
import Web.Routes
import Control.Arrow (first)
import System.Random (randomR, newStdGen)

import qualified Data.Map as Map

import Control.Applicative ((<$>))
import Data.Enumerator (($$), run_)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import System.IO.Unsafe
import Yesod.Content hiding (testSuite)
import Data.Serialize.Get
import Data.Serialize.Put
#else
import Yesod.Content
#endif

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, /not/ subsites. See 'mkYesodSub' for the latter.
-- Use 'parseRoutes' to create the 'Resource's.
mkYesod :: String -- ^ name of the argument datatype
        -> [Resource]
        -> Q [Dec]
mkYesod name = fmap (uncurry (++)) . mkYesodGeneral name [] [] False

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating subsites, /not/ sites. See 'mkYesod' for the latter.
-- Use 'parseRoutes' to create the 'Resource's. In general, a subsite is not
-- executable by itself, but instead provides functionality to
-- be embedded in other sites.
mkYesodSub :: String -- ^ name of the argument datatype
           -> Cxt
           -> [Resource]
           -> Q [Dec]
mkYesodSub name clazzes =
    fmap (uncurry (++)) . mkYesodGeneral name' rest clazzes True
  where
    (name':rest) = words name

-- | Sometimes, you will want to declare your routes in one file and define
-- your handlers elsewhere. For example, this is the only way to break up a
-- monolithic file into smaller parts. Use this function, paired with
-- 'mkYesodDispatch', to do just that.
mkYesodData :: String -> [Resource] -> Q [Dec]
mkYesodData name res = mkYesodDataGeneral name [] False res

mkYesodSubData :: String -> Cxt -> [Resource] -> Q [Dec]
mkYesodSubData name clazzes res = mkYesodDataGeneral name clazzes True res

mkYesodDataGeneral :: String -> Cxt -> Bool -> [Resource] -> Q [Dec]
mkYesodDataGeneral name clazzes isSub res = do
    let (name':rest) = words name
    (x, _) <- mkYesodGeneral name' rest clazzes isSub res
    let rname = mkName $ "resources" ++ name
    eres <- lift res
    let y = [ SigD rname $ ListT `AppT` ConT ''Resource
            , FunD rname [Clause [] (NormalB eres) []]
            ]
    return $ x ++ y

-- | See 'mkYesodData'.
mkYesodDispatch :: String -> [Resource] -> Q [Dec]
mkYesodDispatch name = fmap snd . mkYesodGeneral name [] [] False

mkYesodSubDispatch :: String -> Cxt -> [Resource] -> Q [Dec]
mkYesodSubDispatch name clazzes = fmap snd . mkYesodGeneral name' rest clazzes True 
  where (name':rest) = words name

mkYesodGeneral :: String -- ^ argument name
               -> [String] -- ^ parameters for site argument
               -> Cxt -- ^ classes
               -> Bool -- ^ is subsite?
               -> [Resource]
               -> Q ([Dec], [Dec])
mkYesodGeneral name args clazzes isSub res = do
    let name' = mkName name
        args' = map mkName args
        arg = foldl AppT (ConT name') $ map VarT args'
    th <- mapM (thResourceFromResource arg) res -- FIXME now we cannot have multi-nested subsites
    w' <- createRoutes th
    let routesName = mkName $ name ++ "Route"
    let w = DataD [] routesName [] w' [''Show, ''Read, ''Eq]
    let x = TySynInstD ''Route [arg] $ ConT routesName

    parse' <- createParse th
    parse'' <- newName "parse"
    let parse = LetE [FunD parse'' parse'] $ VarE parse''

    render' <- createRender th
    render'' <- newName "render"
    let render = LetE [FunD render'' render'] $ VarE render''

    tmh <- [|toMasterHandlerDyn|]
    modMaster <- [|fmap chooseRep|]
    dispatch' <- createDispatch modMaster tmh th
    dispatch'' <- newName "dispatch"
    let dispatch = LetE [FunD dispatch'' dispatch'] $ LamE [WildP] $ VarE dispatch''

    site <- [|Site|]
    let site' = site `AppE` dispatch `AppE` render `AppE` parse
    let (ctx, ytyp, yfunc) =
            if isSub
                then (clazzes, ConT ''YesodSubSite `AppT` arg `AppT` VarT (mkName "master"), "getSubSite")
                else ([], ConT ''YesodSite `AppT` arg, "getSite")
    let y = InstanceD ctx ytyp
                [ FunD (mkName yfunc) [Clause [] (NormalB site') []]
                ]
    return ([w, x], [y])

isStatic :: Piece -> Bool
isStatic StaticPiece{} = True
isStatic _ = False

thResourceFromResource :: Type -> Resource -> Q THResource
thResourceFromResource _ (Resource n ps atts)
    | all (all isUpper) atts = return (n, Simple ps atts)
thResourceFromResource master (Resource n ps [stype, toSubArg])
    -- static route to subsite
    = do
        let stype' = ConT $ mkName stype
        gss <- [|getSubSite|]
        let inside = ConT ''Maybe `AppT`
                     (ConT ''GHandler `AppT` stype' `AppT` master `AppT`
                      ConT ''ChooseRep)
        let typ = ConT ''Site `AppT`
                  (ConT ''Route `AppT` stype') `AppT`
                  (ArrowT `AppT` ConT ''String `AppT` inside)
        let gss' = gss `SigE` typ
        parse' <- [|parsePathSegments|]
        let parse = parse' `AppE` gss'
        render' <- [|formatPathSegments|]
        let render = render' `AppE` gss'
        dispatch' <- [|flip handleSite (error "Cannot use subsite render function")|]
        let dispatch = dispatch' `AppE` gss'
        tmg <- mkToMasterArg ps toSubArg
        return (n, SubSite
            { ssType = ConT ''Route `AppT` stype'
            , ssParse = parse
            , ssRender = render
            , ssDispatch = dispatch
            , ssToMasterArg = tmg
            , ssPieces = ps
            })


thResourceFromResource _ (Resource n _ _) =
    error $ "Invalid attributes for resource: " ++ n

mkToMasterArg :: [Piece] -> String -> Q Exp
mkToMasterArg ps fname = do
  let nargs = length $ filter (not.isStatic) ps
      f = VarE $ mkName fname
  args <- sequence $ take nargs $ repeat $ newName "x"
  rsg <- [| runSubsiteGetter|]
  let xps = map VarP args
      xes = map VarE args
      e' = foldl (\x y -> x `AppE` y) f xes
      e = rsg `AppE` e'
  return $ LamE xps e

sessionName :: String
sessionName = "_SESSION"

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This is the same as 'toWaiAppPlain', except it includes three
-- middlewares: GZIP compression, JSON-P and path cleaning. This is the
-- recommended approach for most users.
toWaiApp :: (Yesod y, YesodSite y) => y -> IO (W.Application a)
toWaiApp y = do
    a <- toWaiAppPlain y
    return $ gzip False
           $ jsonp
             a

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This differs from 'toWaiApp' in that it only uses the cleanpath
-- middleware.
toWaiAppPlain :: (Yesod y, YesodSite y) => y -> IO (W.Application a)
toWaiAppPlain a = do
    key' <- if enableClientSessions a
                then Just `fmap` encryptKey a
                else return Nothing
    return $ cleanPath (splitPath a) (B.pack $ approot a)
           $ toWaiApp' a key'

toWaiApp' :: (Yesod y, YesodSite y)
          => y
          -> Maybe Key
          -> [String]
          -> W.Request
          -> IO (W.Response a)
toWaiApp' y key' segments env = do
    now <- getCurrentTime
    let getExpires m = fromIntegral (m * 60) `addUTCTime` now
    let exp' = getExpires $ clientSessionDuration y
    let host = if sessionIpAddress y then W.remoteHost env else ""
    let session' =
            case key' of
                Nothing -> []
                Just key'' -> fromMaybe [] $ do
                    raw <- lookup "Cookie" $ W.requestHeaders env
                    val <- lookup (B.pack sessionName) $ parseCookies raw
                    decodeSession key'' now host val
    let site = getSite
        method = B.unpack $ W.requestMethod env
        types = httpAccept env
        pathSegments = filter (not . null) segments
        eurl = parsePathSegments site pathSegments
        render u qs =
            let (ps, qs') = formatPathSegments site u
             in fromMaybe
                    (joinPath y (approot y) ps $ qs ++ qs')
                    (urlRenderOverride y u)
    let errorHandler' = localNoCurrent . errorHandler
    rr <- parseWaiRequest env session'
    let h = do
          onRequest
          case eurl of
            Left _ -> errorHandler' NotFound
            Right url -> do
                isWrite <- isWriteRequest url
                ar <- isAuthorized url isWrite
                case ar of
                    Authorized -> return ()
                    AuthenticationRequired ->
                        case authRoute y of
                            Nothing ->
                                permissionDenied "Authentication required"
                            Just url' -> do
                                setUltDest'
                                redirect RedirectTemporary url'
                    Unauthorized s -> permissionDenied s
                case handleSite site render url method of
                    Nothing -> errorHandler' $ BadMethod method
                    Just h' -> h'
    let eurl' = either (const Nothing) Just eurl
    let eh er = runHandler (errorHandler' er) render eurl' id y id
    let ya = runHandler h render eurl' id y id
    let sessionMap = Map.fromList
                   $ filter (\(x, _) -> x /= nonceKey) session'
    (s, hs, ct, c, sessionFinal) <- unYesodApp ya eh rr types sessionMap
    let sessionVal =
            case key' of
                Nothing -> B.empty
                Just key'' ->
                     encodeSession key'' exp' host
                   $ Map.toList
                   $ Map.insert nonceKey (reqNonce rr) sessionFinal
    let hs' =
            case key' of
                Nothing -> hs
                Just _ -> AddCookie
                            (clientSessionDuration y)
                            sessionName
                            (bsToChars sessionVal)
                          : hs
        hs'' = map (headerToPair getExpires) hs'
        hs''' = ("Content-Type", charsToBs ct) : hs''
    return $
        case c of
            ContentLBS lbs -> W.ResponseLBS s hs''' lbs
            ContentFile fp -> W.ResponseFile s hs''' fp
            ContentEnum e -> W.ResponseEnumerator $ \iter ->
                run_ $ e $$ iter s hs'''

httpAccept :: W.Request -> [ContentType]
httpAccept = map B.unpack
           . parseHttpAccept
           . fromMaybe B.empty
           . lookup "Accept"
           . W.requestHeaders

-- | Runs an application with CGI if CGI variables are present (namely
-- PATH_INFO); otherwise uses SimpleServer.
basicHandler :: (Yesod y, YesodSite y)
             => Int -- ^ port number
             -> y
             -> IO ()
basicHandler port y = basicHandler' port (Just "localhost") y


-- | Same as 'basicHandler', but allows you to specify the hostname to display
-- to the user. If 'Nothing' is provided, then no output is produced.
basicHandler' :: (Yesod y, YesodSite y)
              => Int -- ^ port number
              -> Maybe String -- ^ host name, 'Nothing' to show nothing
              -> y
              -> IO ()
basicHandler' port mhost y = do
    app <- toWaiApp y
    vars <- getEnvironment
    case lookup "PATH_INFO" vars of
        Nothing -> do
            case mhost of
                Nothing -> return ()
                Just h -> putStrLn $ concat
                    ["http://", h, ":", show port, "/"]
            SS.run port app
        Just _ -> CGI.run app

parseWaiRequest :: W.Request
                -> [(String, String)] -- ^ session
                -> IO Request
parseWaiRequest env session' = do
    let gets' = map (bsToChars *** bsToChars)
              $ parseQueryString $ W.queryString env
    let reqCookie = fromMaybe B.empty $ lookup "Cookie"
                  $ W.requestHeaders env
        cookies' = map (bsToChars *** bsToChars) $ parseCookies reqCookie
        acceptLang = lookup "Accept-Language" $ W.requestHeaders env
        langs = map bsToChars $ maybe [] parseHttpAccept acceptLang
        langs' = case lookup langKey session' of
                    Nothing -> langs
                    Just x -> x : langs
        langs'' = case lookup langKey cookies' of
                    Nothing -> langs'
                    Just x -> x : langs'
        langs''' = case lookup langKey gets' of
                     Nothing -> langs''
                     Just x -> x : langs''
    rbthunk <- iothunk $ rbHelper env
    nonce <- case lookup nonceKey session' of
                Just x -> return x
                Nothing -> do
                    g <- newStdGen
                    return $ fst $ randomString 10 g
    return $ Request gets' cookies' rbthunk env langs''' nonce
  where
    randomString len =
        first (map toChar) . sequence' (replicate len (randomR (0, 61)))
    sequence' [] g = ([], g)
    sequence' (f:fs) g =
        let (f', g') = f g
            (fs', g'') = sequence' fs g'
         in (f' : fs', g'')
    toChar i
        | i < 26 = toEnum $ i + fromEnum 'A'
        | i < 52 = toEnum $ i + fromEnum 'a' - 26
        | otherwise = toEnum $ i + fromEnum '0' - 52

nonceKey :: String
nonceKey = "_NONCE"

rbHelper :: W.Request -> IO RequestBodyContents
rbHelper req =
    (map fix1 *** map fix2) <$> run_ (enum $$ iter)
  where
    enum = W.requestBody req
    iter = parseRequestBody lbsSink req
    fix1 = bsToChars *** bsToChars
    fix2 (x, NWP.FileInfo a b c) =
        (bsToChars x, FileInfo (bsToChars a) (bsToChars b) c)

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
     in ("Set-Cookie", charsToBs
                            $ key ++ "=" ++ value ++"; path=/; expires="
                              ++ formatCookieExpires expires)
headerToPair _ (DeleteCookie key) =
    ("Set-Cookie", charsToBs $
     key ++ "=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT")
headerToPair _ (Header key value) =
    (fromString key, charsToBs value)

encodeSession :: CS.Key
              -> UTCTime -- ^ expire time
              -> B.ByteString -- ^ remote host
              -> [(String, String)] -- ^ session
              -> B.ByteString -- ^ cookie value
encodeSession key expire rhost session' =
    encrypt key $ encode $ SessionCookie expire rhost session'

decodeSession :: CS.Key
              -> UTCTime -- ^ current time
              -> B.ByteString -- ^ remote host field
              -> B.ByteString -- ^ cookie value
              -> Maybe [(String, String)]
decodeSession key now rhost encrypted = do
    decrypted <- decrypt key encrypted
    SessionCookie expire rhost' session' <-
        either (const Nothing) Just $ decode decrypted
    guard $ expire > now
    guard $ rhost' == rhost
    return session'

data SessionCookie = SessionCookie UTCTime B.ByteString [(String, String)]
    deriving (Show, Read)
instance Serialize SessionCookie where
    put (SessionCookie a b c) = putTime a >> put b >> put c
    get = do
        a <- getTime
        b <- Ser.get
        c <- Ser.get
        return $ SessionCookie a b c

putTime :: Putter UTCTime
putTime t@(UTCTime d _) = do
    put $ toModifiedJulianDay d
    let ndt = diffUTCTime t $ UTCTime d 0
    put $ toRational ndt

getTime :: Get UTCTime
getTime = do
    d <- Ser.get
    ndt <- Ser.get
    return $ fromRational ndt `addUTCTime` UTCTime (ModifiedJulianDay d) 0

#if TEST

testSuite :: Test
testSuite = testGroup "Yesod.Dispatch"
    [ testProperty "encode/decode session" propEncDecSession
    , testProperty "get/put time" propGetPutTime
    ]

propEncDecSession :: [(String, String)] -> Bool
propEncDecSession session' = unsafePerformIO $ do
    key <- getDefaultKey
    now <- getCurrentTime
    let expire = addUTCTime 1 now
    let rhost = B.pack "some host"
    let val = encodeSession key expire rhost session'
    return $ Just session' == decodeSession key now rhost val

propGetPutTime :: UTCTime -> Bool
propGetPutTime t = Right t == runGet getTime (runPut $ putTime t)

instance Arbitrary UTCTime where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ addUTCTime (fromRational b)
               $ UTCTime (ModifiedJulianDay a) 0

#endif
