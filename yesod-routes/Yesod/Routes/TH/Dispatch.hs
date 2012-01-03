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
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))

mkDispatchClause :: [Resource]
                 -> Q Clause
mkDispatchClause ress = do
    -- Allocate the names to be used. Start off with the names passed to the
    -- function itself (with a 0 suffix).
    master0 <- newName "master0"
    sub0 <- newName "sub0"
    toMaster0 <- newName "toMaster0"
    app4040 <- newName "app4040"
    handler4050 <- newName "handler4050"
    method0 <- newName "method0"
    pieces0 <- newName "pieces0"

    -- The following names will be used internally. We don't reuse names so as
    -- to avoid shadowing names (triggers warnings with -Wall). Additionally,
    -- we want to ensure that none of the code passed to toDispatch uses
    -- variables from the closure to prevent the dispatch data structure from
    -- being rebuilt on each run.
    master <- newName "master"
    sub <- newName "sub"
    toMaster <- newName "toMaster"
    app404 <- newName "app404"
    handler405 <- newName "handler405"
    method <- newName "method"
    pieces <- newName "pieces"

    -- Name of the dispatch function itself
    dispatch <- newName "dispatch"

    -- The input to the clause.
    let pats = map VarP [master0, sub0, toMaster0, app4040, handler4050, method0, pieces0]

    -- For each resource that dispatches based on methods, build up a map for handling the dispatching.
    methodMaps <- catMaybes <$> mapM buildMethodMap ress

    u <- [|error "mkDispatchClause"|]
    return $ Clause pats (NormalB u) methodMaps

-- | Determine the name of the method map for a given resource name.
methodMapName :: String -> Name
methodMapName s = mkName $ "methods" ++ s

buildMethodMap :: Resource -> Q (Maybe Dec)
buildMethodMap (Resource _ _ (Methods _ [])) = return Nothing -- single handle function
buildMethodMap (Resource name _ (Methods _ methods)) = do
    fromList <- [|Map.fromList|]
    methods' <- mapM go methods
    let exp = fromList `AppE` ListE methods'
    let fun = FunD (methodMapName name) [Clause [] (NormalB exp) []]
    return $ Just fun
  where
    go method = do
        let func = VarE $ mkName $ map toLower method ++ name
        pack' <- [|pack|]
        return $ TupE [pack' `AppE` LitE (StringL method), func]
buildMethodMap (Resource _ _ Subsite{}) = return Nothing

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
-}
