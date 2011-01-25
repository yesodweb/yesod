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
    , toWaiAppPlain
#if TEST
    , dispatchTestSuite
#endif
    ) where

import Yesod.Core
import Yesod.Handler

import Yesod.Request
import Yesod.Internal

import Web.Routes.Quasi
import Web.Routes.Quasi.Parse
import Web.Routes.Quasi.TH
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.Gzip

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as S
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Blaze.ByteString.Builder (toLazyByteString)

import Control.Concurrent.MVar
import Control.Arrow ((***))

import Data.Time

import Control.Monad
import Data.Maybe
import Web.ClientSession
import qualified Web.ClientSession as CS
import Data.Char (isUpper)
import Web.Cookie (parseCookies, SetCookie (..), renderSetCookie)

import Data.Serialize
import qualified Data.Serialize as Ser
import Network.Wai.Parse hiding (FileInfo)
import qualified Network.Wai.Parse as NWP
import Data.String (fromString)
import Web.Routes
import Control.Arrow (first)
import System.Random (randomR, newStdGen)

import qualified Data.Map as Map

import Control.Applicative ((<$>), (<*>))
import Data.Enumerator (($$), run_, Iteratee)
import Control.Monad.IO.Class (liftIO)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import System.IO.Unsafe
#endif

import Yesod.Content

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
    th' <- mapM (thResourceFromResource arg) res -- FIXME now we cannot have multi-nested subsites
    let th = map fst th'
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
    subsiteClauses <- catMaybes <$> mapM sc th'
    nothing <- [|Nothing|]
    let otherMethods =
            if isSub
                then []
                else [ FunD (mkName "dispatchToSubsite")
                        (subsiteClauses ++ [Clause [WildP, WildP, WildP] (NormalB nothing) []])
                     ]
    let y = InstanceD ctx ytyp
                $ FunD (mkName yfunc) [Clause [] (NormalB site') []]
                : otherMethods
    return ([w, x], [y])
  where
    sc ((constr, SubSite { ssPieces = pieces }), Just toSub) = do
        master <- newName "master"
        mkey <- newName "mkey"
        just <- [|Just|]
        (pat', tma', rest) <- mkPat' pieces $ just `AppE` (VarE (mkName toSub) `AppE` VarE master)
        ds <- [|dispatchSubsite|]
        let body' = ds `AppE` VarE master `AppE` VarE mkey `AppE` rest
        fmap' <- [|(<$>)|]
        let body = InfixE (Just body') fmap' $ Just tma'
        return $ Just $ Clause
            [ VarP master
            , VarP mkey
            , pat'
            ] (NormalB body) []
    sc _ = return Nothing
    mkPat' :: [Piece] -> Exp -> Q (Pat, Exp, Exp)
    mkPat' (MultiPiece _:_) _ = error "MultiPiece not allowed as part of a subsite"
    mkPat' (StaticPiece s:rest) tma = do
        (x, tma, rest') <- mkPat' rest tma
        let sp = LitP $ StringL s
        return (InfixP sp (mkName ":") x, tma, rest')
    mkPat' (SinglePiece s:rest) tma = do
        fsp <- [|either (const Nothing) Just . fromSinglePiece|]
        v <- newName $ "var" ++ s
        be <- [|(<*>)|]
        let tma' = InfixE (Just tma) be $ Just $ fsp `AppE` VarE v
        (x, tma'', rest) <- mkPat' rest tma'
        return (InfixP (VarP v) (mkName ":") x, tma'', rest)
    mkPat' [] parse = do
        rest <- newName "rest"
        return (VarP rest, parse, VarE rest)

isStatic :: Piece -> Bool
isStatic StaticPiece{} = True
isStatic _ = False

thResourceFromResource :: Type -> Resource -> Q (THResource, Maybe String)
thResourceFromResource _ (Resource n ps atts)
    | all (all isUpper) atts = return ((n, Simple ps atts), Nothing)
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
        return ((n, SubSite
            { ssType = ConT ''Route `AppT` stype'
            , ssParse = parse
            , ssRender = render
            , ssDispatch = dispatch
            , ssToMasterArg = tmg
            , ssPieces = ps
            }), Just toSubArg)


thResourceFromResource _ (Resource n _ _) =
    error $ "Invalid attributes for resource: " ++ n

mkToMasterArg :: [Piece] -> String -> Q Exp
mkToMasterArg ps fname = do
  let nargs = length $ filter (not.isStatic) ps
      f = VarE $ mkName fname
  args <- sequence $ take nargs $ repeat $ newName "x"
  rsg <- [|error "runSubsiteGetter"|]
  let xps = map VarP args
      xes = map VarE args
      e' = foldl (\x y -> x `AppE` y) f xes
      e = rsg `AppE` e'
  return $ rsg -- FIXME LamE xps e

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This is the same as 'toWaiAppPlain', except it includes three
-- middlewares: GZIP compression, JSON-P and path cleaning. This is the
-- recommended approach for most users.
toWaiApp :: (Yesod y, YesodSite y) => y -> IO W.Application
toWaiApp y = do
    a <- toWaiAppPlain y
    return $ gzip False
           $ jsonp
             a

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This differs from 'toWaiApp' in that it uses no middlewares.
toWaiAppPlain :: (Yesod y, YesodSite y) => y -> IO W.Application
toWaiAppPlain a = do
    key' <- encryptKey a
    return $ toWaiApp' a key'

toWaiApp' :: (Yesod y, YesodSite y)
          => y
          -> Maybe Key
          -> W.Application
toWaiApp' y key' env = do
    let segments =
            case decodePathInfo $ B.unpack $ W.pathInfo env of
                "":x -> x
                x -> x
    liftIO $ print (W.pathInfo env, segments)
    case dispatchToSubsite y key' segments of
        Nothing ->
            case cleanPath y segments of
                Nothing -> normalDispatch y key' segments env
                Just segments' ->
                    let dest = joinPath y (approot y) segments' []
                        dest' =
                            if S.null (W.queryString env)
                                then dest
                                else S.concat
                                        [ dest
                                        , B.singleton '?'
                                        , W.queryString env
                                        ]
                     in return $ W.responseLBS W.status301
                            [ ("Content-Type", "text/plain")
                            , ("Location", dest')
                            ] "Redirecting"
        Just app -> app env

normalDispatch :: (Yesod m, YesodSite m)
               => m -> Maybe Key -> [String]
               -> W.Application
normalDispatch y key' segments env =
    yesodRunner y key' murl handler env
  where
    method = B.unpack $ W.requestMethod env
    murl = either (const Nothing) Just $ parsePathSegments (getSite' y) segments
    handler =
        case murl of
            Nothing -> notFound
            Just url ->
                case handleSite (getSite' y) (yesodRender y) url method of
                    Nothing -> badMethod
                    Just h -> h

#if TEST

dispatchTestSuite :: Test
dispatchTestSuite = testGroup "Yesod.Dispatch"
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
