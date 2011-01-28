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
import Data.Char (isUpper, toLower)
import Web.Cookie (parseCookies, SetCookie (..), renderSetCookie)

import Data.Serialize
import qualified Data.Serialize as Ser
import Network.Wai.Parse hiding (FileInfo)
import qualified Network.Wai.Parse as NWP
import Data.String (fromString)
import Web.Routes (decodePathInfo)
import Control.Arrow (first)
import System.Random (randomR, newStdGen)

import qualified Data.Map as Map

import Control.Applicative ((<$>), (<*>))
import Data.Enumerator (($$), run_, Iteratee)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')

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

mkYesodGeneral :: String -- ^ foundation name
               -> [String] -- ^ parameters for foundation
               -> Cxt -- ^ classes
               -> Bool -- ^ is subsite?
               -> [Resource]
               -> Q ([Dec], [Dec])
mkYesodGeneral name args clazzes isSub res = do
    let name' = mkName name
        args' = map mkName args
        arg = foldl AppT (ConT name') $ map VarT args'
    th' <- mapM (thResourceFromResource arg) res
    let th = map fst th'
    w' <- createRoutes th
    let routesName = mkName $ name ++ "Route"
    let w = DataD [] routesName [] w' [''Show, ''Read, ''Eq]
    let x = TySynInstD ''Route [arg] $ ConT routesName

    render' <- createRender th
    render'' <- newName "render"
    let render = LetE [FunD render'' render'] $ VarE render''
    let x' = InstanceD [] (ConT ''RenderRoute `AppT` ConT routesName)
                [ FunD (mkName "renderRoute") render'
                ]

    tmh <- [|toMasterHandlerDyn|]
    modMaster <- [|fmap chooseRep|]
    dispatch' <- createDispatch modMaster tmh th
    dispatch'' <- newName "dispatch"
    let dispatch = LetE [FunD dispatch'' dispatch'] $ LamE [WildP] $ VarE dispatch''

    {- FIXME
    let (ctx, ytyp, yfunc) =
            if isSub
                then (clazzes, ConT ''YesodSubSite `AppT` arg `AppT` VarT (mkName "master"), "getSubSite")
                else ([], ConT ''YesodSite `AppT` arg, "getSite")
    -}
    let sortedRes = filter (not . isSubSite) th' ++ filter isSubSite th'
    yd <- mkYesodDispatch' sortedRes
    nothing <- [|Nothing|]
    let master = mkName "master"
    let ctx = ClassP (mkName "Yesod") [VarT master] : clazzes
    let mkYSS = InstanceD ctx (ConT ''YesodDispatch `AppT` arg `AppT` VarT master)
                [ FunD (mkName "yesodDispatch") [yd]
                ]
        mkYS = InstanceD [] (ConT ''YesodDispatch `AppT` arg `AppT` arg) [FunD (mkName "yesodDispatch") [yd]]
    let y = if isSub then mkYSS else mkYS {-InstanceD ctx ytyp
                $ FunD (mkName yfunc) [Clause [] (NormalB site') []]
                : otherMethods -}
    return ([w, x, x'], [y])

isSubSite ((_, SubSite{}), _) = True
isSubSite _ = False

mkYesodDispatch' sortedRes = do
    sub <- newName "sub"
    master <- newName "master"
    mkey <- newName "mkey"
    segments <- newName "segments"
    toMasterRoute <- newName "toMasterRoute"
    nothing <- [|Nothing|]
    body <- foldM (go master sub toMasterRoute mkey segments) nothing sortedRes
    return $ Clause
        [VarP sub, VarP mkey, VarP segments, VarP master, VarP toMasterRoute]
        (NormalB body)
        []
  where
    go master sub toMasterRoute mkey segments onFail ((constr, SubSite { ssPieces = pieces }), Just toSub) = do
        test <- mkSubsiteExp segments pieces id (master, sub, toMasterRoute, mkey, constr, toSub)
        just <- [|Just|]
        app <- newName "app"
        return $ CaseE test
            [ Match (ConP (mkName "Nothing") []) (NormalB onFail) []
            , Match (ConP (mkName "Just") [VarP app]) (NormalB $ VarE app) []
            ]
    go master sub toMasterRoute mkey segments onFail ((constr, Simple pieces methods), Nothing) = do
        test <- mkSimpleExp segments pieces id (master, sub, toMasterRoute, mkey, constr, methods)
        just <- [|Just|]
        app <- newName "app"
        return $ CaseE test
            [ Match (ConP (mkName "Nothing") []) (NormalB onFail) []
            , Match (ConP (mkName "Just") [VarP app]) (NormalB $ just `AppE` VarE app) []
            ]

mkSimpleExp segments [] frontVars (master, sub, toMasterRoute, mkey, constr, methods) = do
    just <- [|Just|]
    nothing <- [|Nothing|]
    onSuccess <- newName "onSuccess"
    req <- newName "req"
    badMethod' <- [|badMethod|]
    rm <- [|W.requestMethod|]
    let caseExp = rm `AppE` VarE req
    yr <- [|yesodRunner|]
    cr <- [|fmap chooseRep|]
    let url = foldl' AppE (ConE $ mkName constr) $ frontVars []
    let runHandlerVars h = runHandler $ foldl' AppE (cr `AppE` (VarE $ mkName h)) $ frontVars []
        runHandler h = NormalB $ yr `AppE` VarE sub
                                    `AppE` VarE master
                                    `AppE` VarE toMasterRoute
                                    `AppE` VarE mkey
                                    `AppE` (just `AppE` url)
                                    `AppE` h
                                    `AppE` VarE req
    let match m = Match (LitP $ StringL m) (runHandlerVars $ map toLower m ++ constr) []
    let clauses =
            case methods of
                [] -> [Clause [] (runHandlerVars $ "handle" ++ constr) []]
                _ -> [Clause [VarP req] (NormalB $ CaseE caseExp $ map match methods ++
                                                                   [Match WildP (runHandler badMethod') []]) []]
    let exp = CaseE (VarE segments)
                [ Match
                    (ConP (mkName "[]") [])
                    (NormalB $ just `AppE` VarE onSuccess)
                    [FunD onSuccess clauses]
                , Match
                    WildP
                    (NormalB nothing)
                    []
                ]
    return exp
mkSimpleExp segments (StaticPiece s:pieces) frontVars x = do
    srest <- newName "segments"
    innerExp <- mkSimpleExp srest pieces frontVars x
    nothing <- [|Nothing|]
    let exp = CaseE (VarE segments)
                [ Match
                    (InfixP (LitP $ StringL s) (mkName ":") (VarP srest))
                    (NormalB innerExp)
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp
mkSimpleExp segments (SinglePiece s:pieces) frontVars x = do
    srest <- newName "segments"
    next' <- newName "next'"
    innerExp <- mkSimpleExp srest pieces (frontVars . (:) (VarE next')) x
    nothing <- [|Nothing|]
    next <- newName "next"
    fsp <- [|fromSinglePiece|]
    let exp' = CaseE (fsp `AppE` VarE next)
                [ Match
                    (ConP (mkName "Left") [WildP])
                    (NormalB nothing)
                    []
                , Match
                    (ConP (mkName "Right") [VarP next'])
                    (NormalB innerExp)
                    []
                ]
    let exp = CaseE (VarE segments)
                [ Match
                    (InfixP (VarP next) (mkName ":") (VarP srest))
                    (NormalB exp')
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp

mkSubsiteExp segments [] frontVars (master, sub, toMasterRoute, mkey, constr, toSub) = do
    yd <- [|yesodDispatch|]
    let con = foldl' AppE (ConE $ mkName constr) $ frontVars []
    let s' = VarE (mkName toSub) `AppE` VarE master
    let s = foldl' AppE s' $ frontVars []
    let app = yd `AppE` s
                 `AppE` VarE mkey
                 `AppE` VarE segments
                 `AppE` VarE master
                 `AppE` con
    just <- [|Just|]
    return $ just `AppE` app
mkSubsiteExp segments (StaticPiece s:pieces) frontVars x = do
    srest <- newName "segments"
    innerExp <- mkSubsiteExp srest pieces frontVars x
    nothing <- [|Nothing|]
    let exp = CaseE (VarE segments)
                [ Match
                    (InfixP (LitP $ StringL s) (mkName ":") (VarP srest))
                    (NormalB innerExp)
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp
mkSubsiteExp segments (SinglePiece s:pieces) frontVars x = do
    srest <- newName "segments"
    next' <- newName "next'"
    innerExp <- mkSubsiteExp srest pieces (frontVars . (:) (VarE next')) x
    nothing <- [|Nothing|]
    next <- newName "next"
    fsp <- [|fromSinglePiece|]
    let exp' = CaseE (fsp `AppE` VarE next)
                [ Match
                    (ConP (mkName "Left") [WildP])
                    (NormalB nothing)
                    []
                , Match
                    (ConP (mkName "Right") [VarP next'])
                    (NormalB innerExp)
                    []
                ]
    let exp = CaseE (VarE segments)
                [ Match
                    (InfixP (VarP next) (mkName ":") (VarP srest))
                    (NormalB exp')
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp

{-
    mkPat' (SinglePiece s:rest) url = do
        fsp <- [|either (const Nothing) Just . fromSinglePiece|]
        v <- newName $ "var" ++ s
        be <- [|(<*>)|]
        let url' = InfixE (Just url) be $ Just $ fsp `AppE` VarE v
        (x, rest, url'') <- mkPat' rest url'
        return (InfixP (VarP v) (mkName ":") x, rest, url'')
    mkPat' [] url = do
        rest <- newName "rest"
        return (VarP rest, VarE rest, url)
-}

mkDispatchLocal ((constr, Simple pieces methods), Nothing) = do
    master <- newName "master"
    mkey <- newName "mkey"
    req <- newName "req"
    just <- [|Just|]
    (pat', rest, url) <- mkPat' pieces $ just `AppE` (ConE $ mkName constr)
    goodParse <- (`AppE` url) <$> [|isJust|]
    tma'' <- (`AppE` url) <$> [|fromJust|]
    nothing <- [|Nothing|]
    let body = if null methods
                    then VarE $ mkName $ "handle" ++ constr
                    else CaseE (VarE req) $ map mkMatch methods ++ [Match WildP (NormalB nothing) []]
    return $ Just $ Clause
        [ VarP master
        , VarP mkey
        , pat'
        ] (GuardedB [(NormalG goodParse, body)]) [] -- FIXME
  where
    singleToMApp :: GHandler s m c -> Maybe W.Application
    singleToMApp = undefined
    multiToMApp = undefined
    -- FIXME requires OverloadedStrings
    mkMatch method = Match (LitP $ StringL method) (NormalB $ VarE $ mkName $ map toLower method ++ constr) []
    mkPat' :: [Piece] -> Exp -> Q (Pat, Exp, Exp)
    mkPat' (StaticPiece s:rest) url = do
        (x, rest', url') <- mkPat' rest url
        let sp = LitP $ StringL s
        return (InfixP sp (mkName ":") x, rest', url')
    mkPat' (SinglePiece s:rest) url = do
        fsp <- [|either (const Nothing) Just . fromSinglePiece|]
        v <- newName $ "var" ++ s
        be <- [|(<*>)|]
        let url' = InfixE (Just url) be $ Just $ fsp `AppE` VarE v
        (x, rest, url'') <- mkPat' rest url'
        return (InfixP (VarP v) (mkName ":") x, rest, url'')
    mkPat' [] url = do
        rest <- newName "rest"
        return (VarP rest, VarE rest, url)
mkDispatchLocal _ = return Nothing

mkDispatchToSubsite ((constr, SubSite { ssPieces = pieces }), Just toSub) = do
    master <- newName "master"
    mkey <- newName "mkey"
    just <- [|Just|]
    (pat', tma', rest, toMaster)
        <- mkPat' pieces
           (ConE $ mkName constr)
         $ just `AppE` (VarE (mkName toSub) `AppE` VarE master)
    ds <- error "FIXME" -- [|dispatchSubsite|]
    goodParse <- (`AppE` tma') <$> [|isJust|]
    tma'' <- (`AppE` tma') <$> [|fromJust|]
    let body' = ds `AppE` VarE master `AppE` VarE mkey `AppE` rest `AppE` toMaster
    fmap' <- [|(<$>)|]
    let body = InfixE (Just body') fmap' $ Just tma'
    return $ Just $ Clause
        [ VarP master
        , VarP mkey
        , pat'
        ] (GuardedB [(NormalG goodParse, body)]) []
  where
    mkPat' :: [Piece] -> Exp -> Exp -> Q (Pat, Exp, Exp, Exp)
    mkPat' (MultiPiece _:_) _ _ = error "MultiPiece not allowed as part of a subsite"
    mkPat' (StaticPiece s:rest) toMaster tma = do
        (x, tma', rest', toMaster') <- mkPat' rest toMaster tma
        let sp = LitP $ StringL s
        return (InfixP sp (mkName ":") x, tma', rest', toMaster')
    mkPat' (SinglePiece s:rest) toMaster tma = do
        fsp <- [|either (const Nothing) Just . fromSinglePiece|]
        v <- newName $ "var" ++ s
        be <- [|(<*>)|]
        let tma' = InfixE (Just tma) be $ Just $ fsp `AppE` VarE v
        let toMaster' = toMaster `AppE` VarE v
        (x, tma'', rest, toMaster'') <- mkPat' rest toMaster' tma'
        return (InfixP (VarP v) (mkName ":") x, tma'', rest, toMaster'')
    mkPat' [] toMaster parse = do
        rest <- newName "rest"
        return (VarP rest, parse, VarE rest, toMaster)
mkDispatchToSubsite _ = return Nothing

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
        {-
        gss <- [|error "FIXME getSubSite"|]
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
        -}
        parse <- [|error "ssParse"|]
        dispatch <- [|error "ssDispatch"|]
        render <- [|renderRoute|]
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
toWaiApp :: (Yesod y, YesodDispatch y y) => y -> IO W.Application
toWaiApp y = do
    a <- toWaiAppPlain y
    return $ gzip False
           $ jsonp
             a

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This differs from 'toWaiApp' in that it uses no middlewares.
toWaiAppPlain :: (Yesod y, YesodDispatch y y) => y -> IO W.Application
toWaiAppPlain a = do
    key' <- encryptKey a
    return $ toWaiApp' a key'

toWaiApp' :: (Yesod y, YesodDispatch y y)
          => y
          -> Maybe Key
          -> W.Application
toWaiApp' y key' env = do
    let segments =
            case decodePathInfo $ B.unpack $ W.pathInfo env of
                "":x -> x
                x -> x
    liftIO $ print (W.pathInfo env, segments)
    case yesodDispatch y key' segments y id of
        Just app -> app env
        Nothing ->
            case cleanPath y segments of
                Nothing ->
                    case yesodDispatch y key' segments y id of
                        Just app -> app env
                        Nothing -> yesodRunner y y id key' Nothing notFound env
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

{-
defaultDispatchSubsite
    :: (Yesod m, YesodDispatch m, YesodSubSite s m)
    => m -> Maybe Key -> [String]
    -> (Route s -> Route m)
    -> s
    -> W.Application
defaultDispatchSubsite y key' segments toMasterRoute s env = error "FIXME" {-
    case dispatchToSubSubsite y key' segments toMasterRoute s of
        Just app -> app env
        Nothing ->
            case dispatchSubLocal y key' segments toMasterRoute s of
                Just app -> app env
                Nothing -> yesodRunner y key' Nothing notFound env-}
-}

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
