{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.Dispatch
    ( -- ** Dispatch
      mkDispatchClause
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Data.Maybe (maybeToList, catMaybes)
import Control.Monad (replicateM)
import Data.Text (pack)
import qualified Yesod.Routes.Dispatch as D
import qualified Data.Map as Map
import Data.Char (toLower)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class

mkDispatchClause :: [Resource]
                 -> Q Clause
mkDispatchClause ress = undefined
{- FIXME
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

mkTsPattern :: [Piece] -> Maybe t -> Q ([Name], Maybe Name, Pat)
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
-}

-- | Convert a 'Piece' into a 'D.Piece'.
toPiece :: Piece -> Q Exp
toPiece (Static s) = [|D.Static $ pack $(lift s)|]
toPiece Dynamic{} = [|D.Dynamic|]

-- | Convert a 'Resource' into a 'D.Route'.
toRoute :: Resource -> Q Exp
toRoute res = do
    let ps = fmap ListE $ mapM toPiece $ resourcePieces res
    let m = maybe [|False|] (const [|True|]) $ resourceMulti res
    case resourceDispatch res of
        Methods mmulti mds -> do
            let toPair m' = do
                    key <- [|pack $(lift m')|]
                    let value = VarE $ mkName $ map toLower m' ++ resourceName res
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
