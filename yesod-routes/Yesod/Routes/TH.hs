{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH
    ( -- * Data types
      Resource (..)
    , Piece (..)
    , Dispatch (..)
      -- * Functions
      -- ** Route data type
    , mkRouteType
    , mkRouteCons
      -- ** RenderRoute
    , mkRenderRouteClauses
    , mkRenderRouteInstance
      -- ** Dispatch
    , mkDispatchClause
    ) where

import Language.Haskell.TH.Syntax
import Yesod.Core
    ( Route, RenderRoute (renderRoute), toSinglePiece, toMultiPiece
    )
import Data.Maybe (maybeToList, catMaybes)
import Control.Monad (replicateM)
import Data.Text (pack)
import qualified Yesod.Routes.Dispatch as D
import qualified Data.Map as Map
import Data.Char (toLower)

data Resource = Resource
    { resourceName :: String
    , resourcePieces :: [Piece]
    , resourceDispatch :: Dispatch
    }

data Piece = Static String | Dynamic Type

data Dispatch = Methods (Maybe Type) [String] | Subsite
    { subsiteType :: Type
    , subsiteFunc :: String
    }

resourceMulti Resource { resourceDispatch = Methods (Just t) _ } = Just t
resourceMulti _ = Nothing

mkRouteCons :: [Resource] -> [Con]
mkRouteCons =
    map mkRouteCon
  where
    mkRouteCon res =
        NormalC (mkName $ resourceName res)
            $ map (\x -> (NotStrict, x))
            $ concat [singles, multi, sub]
      where
        singles = concatMap toSingle $ resourcePieces res
        toSingle Static{} = []
        toSingle (Dynamic typ) = [typ]

        multi = maybeToList $ resourceMulti res

        sub =
            case resourceDispatch res of
                Subsite { subsiteType = typ } -> [ConT ''Route `AppT` typ]
                _ -> []

mkRouteType :: String -> [Resource] -> Dec
mkRouteType name res =
    DataD [] (mkName name) [] (mkRouteCons res) clazzes
  where
    clazzes = [''Show, ''Eq, ''Read]

mkRenderRouteClauses :: [Resource] -> Q [Clause]
mkRenderRouteClauses =
    mapM go
  where
    isDynamic Dynamic{} = True
    isDynamic _ = False

    go res = do
        let cnt = length (filter isDynamic $ resourcePieces res) + maybe 0 (const 1) (resourceMulti res)
        dyns <- replicateM cnt $ newName "dyn"
        sub <-
            case resourceDispatch res of
                Subsite{} -> fmap return $ newName "sub"
                _ -> return []
        let pat = ConP (mkName $ resourceName res) $ map VarP $ dyns ++ sub

        pack <- [|pack|]
        tsp <- [|toSinglePiece|]
        let piecesSingle = mkPieces (AppE pack . LitE . StringL) tsp (resourcePieces res) dyns

        piecesMulti <-
            case resourceMulti res of
                Nothing -> return $ ListE []
                Just{} -> do
                    tmp <- [|toMultiPiece|]
                    return $ tmp `AppE` VarE (last dyns)

        body <-
            case sub of
                [x] -> do
                    rr <- [|renderRoute|]
                    a <- newName "a"
                    b <- newName "b"

                    colon <- [|(:)|]
                    let cons a b = InfixE (Just a) colon (Just b)
                    let pieces = foldr cons (VarE a) piecesSingle

                    return $ LamE [TupP [VarP a, VarP b]] (TupE [pieces, VarE b]) `AppE` (rr `AppE` VarE x)
                _ -> do
                    colon <- [|(:)|]
                    let cons a b = InfixE (Just a) colon (Just b)
                    return $ TupE [foldr cons piecesMulti piecesSingle, ListE []]

        return $ Clause [pat] (NormalB body) []

    mkPieces _ _ [] _ = []
    mkPieces toText tsp (Static s:ps) dyns = toText s : mkPieces toText tsp ps dyns
    mkPieces toText tsp (Dynamic{}:ps) (d:dyns) = tsp `AppE` VarE d : mkPieces toText tsp ps dyns

mkRenderRouteInstance :: String -> [Resource] -> Q Dec
mkRenderRouteInstance name ress = do
    cls <- mkRenderRouteClauses ress
    return $ InstanceD [] (ConT ''RenderRoute `AppT` ConT (mkName name))
        [ FunD (mkName "renderRoute") cls
        ]

mkDispatchClause :: [Resource]
                 -> Q Exp -- ^ convert handler to application
                 -> Q Clause
mkDispatchClause ress toApp = do
    let routes = fmap ListE $ mapM toRoute ress
    sub <- newName "sub"
    mkey <- newName "mkey"
    ts <- newName "ts"
    master <- newName "master"
    toMaster <- newName "toMaster"
    let pats =
            [ VarP sub
            , VarP mkey
            , VarP ts
            , VarP master
            , VarP toMaster
            ]

    dispatch <- newName "dispatch"
    body <- [|D.toDispatch $(routes)|]
    return $ Clause
        pats
        (NormalB $ VarE dispatch `AppE` VarE ts `AppE` TupE (map VarE [sub, mkey, master, toMaster]))
        [FunD dispatch [Clause [] (NormalB body) []]]
  where
    toRoute :: Resource -> Q Exp
    toRoute res = do
        let ps = fmap ListE $ mapM toPiece $ resourcePieces res
        let m = maybe [|False|] (const [|True|]) $ resourceMulti res
        case resourceDispatch res of
            Methods mmulti mds -> do
                let toPair m = do
                        key <- [|pack $(lift m)|]
                        let value = VarE $ mkName $ map toLower m ++ resourceName res
                        return $ TupE [key, value]
                let handler =
                        if null mds
                            then [|Left $(return $ VarE $ mkName $ "handle" ++ resourceName res)|]
                            else [|Right $ Map.fromList $(fmap ListE $ mapM toPair mds)|]
                sub <- newName "sub"
                mkey <- newName "mkey"
                (dyns, mend, tsPattern) <- mkTsPattern (resourcePieces res) mmulti
                master <- newName "master"
                toMaster <- newName "toMaster"
                body <- [|$(toApp) $(handler)|]
                let func = LamE
                            [ tsPattern
                            , TupP
                                [ VarP sub
                                , VarP mkey
                                , VarP master
                                , VarP toMaster
                                ]
                            ]
                            body
                [|D.Route $(ps) $(m) $(return func)|]
            Subsite _ func -> [|D.Route $(ps) $(m) $ $(toApp) $(return $ VarE $ mkName $ "handle" ++ resourceName res)|] -- FIXME

    toPiece :: Piece -> Q Exp
    toPiece (Static s) = [|D.Static $ pack $(lift s)|]
    toPiece Dynamic{} = [|D.Dynamic|]

mkTsPattern pieces mmulti = do
    end <-
        case mmulti of
            Nothing -> return (Nothing, ConP (mkName "[]") [])
            Just{} -> do
                end <- newName "end"
                return (Just end, VarP end)
    pieces' <- mapM go pieces
    return (catMaybes $ map fst pieces', fst end, foldr (flip InfixP $ mkName ":") (snd end) $ map snd pieces')
  where
    go Static{} = return (Nothing, WildP)
    go Dynamic{} = do
        dyn <- newName "dyn"
        return (Just dyn, VarP dyn)
