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

import Prelude hiding (exp)
import Yesod.Core
import Yesod.Handler

import Web.Routes.Quasi
import Web.Routes.Quasi.Parse
import Web.Routes.Quasi.TH
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.Gzip

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as S
import Data.ByteString.Lazy.Char8 ()

import Control.Monad
import Web.ClientSession
import Data.Char (isUpper, toLower)

import Web.Routes (decodePathInfo)

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
    th' <- mapM thResourceFromResource res
    let th = map fst th'
    w' <- createRoutes th
    let routesName = mkName $ name ++ "Route"
    let w = DataD [] routesName [] w' [''Show, ''Read, ''Eq]
    let x = TySynInstD ''Route [arg] $ ConT routesName

    render <- createRender th
    let x' = InstanceD [] (ConT ''RenderRoute `AppT` ConT routesName)
                [ FunD (mkName "renderRoute") render
                ]

    let sortedRes = filter (not . isSubSite) th' ++ filter isSubSite th'
    yd <- mkYesodDispatch' sortedRes
    let master = mkName "master"
    let ctx = if isSub
                then ClassP (mkName "Yesod") [VarT master] : clazzes
                else []
    let ytyp = if isSub
                then ConT ''YesodDispatch `AppT` arg `AppT` VarT master
                else ConT ''YesodDispatch `AppT` arg `AppT` arg
    let y = InstanceD ctx ytyp [FunD (mkName "yesodDispatch") [yd]]
    return ([w, x, x'], [y])

isSubSite :: ((String, Pieces), a) -> Bool
isSubSite ((_, SubSite{}), _) = True
isSubSite _ = False

mkYesodDispatch' :: [((String, Pieces), Maybe String)] -> Q Clause
mkYesodDispatch' sortedRes = do
    sub <- newName "sub"
    master <- newName "master"
    mkey <- newName "mkey"
    segments <- newName "segments"
    toMasterRoute <- newName "toMasterRoute"
    nothing <- [|Nothing|]
    body <- foldM (go master (VarE sub) (VarE toMasterRoute) mkey segments) nothing sortedRes
    return $ Clause
        [VarP sub, VarP mkey, VarP segments, VarP master, VarP toMasterRoute]
        (NormalB body)
        []
  where
    go master sub toMasterRoute mkey segments onFail ((constr, SubSite { ssPieces = pieces }), Just toSub) = do
        test <- mkSubsiteExp segments pieces id (master, sub, toMasterRoute, mkey, constr, VarE $ mkName toSub)
        app <- newName "app"
        return $ CaseE test
            [ Match (ConP (mkName "Nothing") []) (NormalB onFail) []
            , Match (ConP (mkName "Just") [VarP app]) (NormalB $ VarE app) []
            ]
    go master sub toMasterRoute mkey segments onFail ((constr, Simple pieces methods), Nothing) = do
        test <- mkSimpleExp (VarE segments) pieces id (master, sub, toMasterRoute, mkey, constr, methods)
        just <- [|Just|]
        app <- newName "app"
        return $ CaseE test
            [ Match (ConP (mkName "Nothing") []) (NormalB onFail) []
            , Match (ConP (mkName "Just") [VarP app]) (NormalB $ just `AppE` VarE app) []
            ]
    go _ _ _ _ _ _ _ = error "Invalid combination"

mkSimpleExp :: Exp -- ^ segments
            -> [Piece]
            -> ([Exp] -> [Exp]) -- ^ variables already parsed
            -> (Name, Exp, Exp, Name, String, [String]) -- ^ master, sub, toMasterRoute, mkey, constructor, methods
            -> Q Exp
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
    let runHandlerVars h = runHandler' $ cr `AppE` foldl' AppE (VarE $ mkName h) (frontVars [])
        runHandler' h = NormalB $ yr `AppE` sub
                                     `AppE` VarE master
                                     `AppE` toMasterRoute
                                     `AppE` VarE mkey
                                     `AppE` (just `AppE` url)
                                     `AppE` h
                                     `AppE` VarE req
    let match m = Match (LitP $ StringL m) (runHandlerVars $ map toLower m ++ constr) []
    let clauses =
            case methods of
                [] -> [Clause [VarP req] (runHandlerVars $ "handle" ++ constr) []]
                _ -> [Clause [VarP req] (NormalB $ CaseE caseExp $ map match methods ++
                                                                   [Match WildP (runHandler' badMethod') []]) []]
    let exp = CaseE segments
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
    innerExp <- mkSimpleExp (VarE srest) pieces frontVars x
    nothing <- [|Nothing|]
    let exp = CaseE segments
                [ Match
                    (InfixP (LitP $ StringL s) (mkName ":") (VarP srest))
                    (NormalB innerExp)
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp
mkSimpleExp segments (SinglePiece _:pieces) frontVars x = do
    srest <- newName "segments"
    next' <- newName "next'"
    innerExp <- mkSimpleExp (VarE srest) pieces (frontVars . (:) (VarE next')) x
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
    let exp = CaseE segments
                [ Match
                    (InfixP (VarP next) (mkName ":") (VarP srest))
                    (NormalB exp')
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp
mkSimpleExp segments [MultiPiece _] frontVars x = do
    next' <- newName "next'"
    srest <- [|[]|]
    innerExp <- mkSimpleExp srest [] (frontVars . (:) (VarE next')) x
    nothing <- [|Nothing|]
    fmp <- [|fromMultiPiece|]
    let exp = CaseE (fmp `AppE` segments)
                [ Match
                    (ConP (mkName "Left") [WildP])
                    (NormalB nothing)
                    []
                , Match
                    (ConP (mkName "Right") [VarP next'])
                    (NormalB innerExp)
                    []
                ]
    return exp
mkSimpleExp _ (MultiPiece _:_) _ _ = error "MultiPiece must be last piece"

mkSubsiteExp :: Name -- ^ segments
             -> [Piece]
             -> ([Exp] -> [Exp]) -- ^ variables already parsed
             -> (Name, Exp, Exp, Name, String, Exp) -- ^ master, sub, toMasterRoute, mkey, constructor, toSub
             -> Q Exp
mkSubsiteExp segments [] frontVars (master, sub, toMasterRoute, mkey, constr, toSub) = do
    yd <- [|yesodDispatch|]
    dot <- [|(.)|]
    let con = InfixE (Just toMasterRoute) dot $ Just $ foldl' AppE (ConE $ mkName constr) $ frontVars []
    -- proper handling for sub-subsites
    let sub' = foldl' AppE (toSub `AppE` sub) $ frontVars []
    let app = yd `AppE` sub'
                 `AppE` VarE mkey
                 `AppE` VarE segments
                 `AppE` VarE master
                 `AppE` con
    just <- [|Just|]
    return $ just `AppE` app
mkSubsiteExp _ (MultiPiece _:_) _ _ = error "Subsites cannot have MultiPiece"
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
mkSubsiteExp segments (SinglePiece _:pieces) frontVars x = do
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

thResourceFromResource :: Resource -> Q (THResource, Maybe String)
thResourceFromResource (Resource n ps atts)
    | all (all isUpper) atts = return ((n, Simple ps atts), Nothing)
thResourceFromResource (Resource n ps [stype, toSubArg]) = do
    let stype' = ConT $ mkName stype
    parse <- [|error "ssParse"|]
    dispatch <- [|error "ssDispatch"|]
    render <- [|renderRoute|]
    tmg <- [|error "ssToMasterArg"|]
    return ((n, SubSite
        { ssType = ConT ''Route `AppT` stype'
        , ssParse = parse
        , ssRender = render
        , ssDispatch = dispatch
        , ssToMasterArg = tmg
        , ssPieces = ps
        }), Just toSubArg)

thResourceFromResource (Resource n _ _) =
    error $ "Invalid attributes for resource: " ++ n

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
