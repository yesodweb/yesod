{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-} -- QuasiQuoter
module Yesod.Internal.RouteParsing
    ( createRoutes
    , createRender
    , createParse
    , createDispatch
    , Pieces (..)
    , THResource
    , parseRoutes
    , parseRoutesFile
    , parseRoutesNoCheck
    , parseRoutesFileNoCheck
    , Resource (..)
    , Piece (..)
    ) where

import Web.PathPieces
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.Either
import Data.List
import Data.Char (toLower)
import qualified Data.Text
import Language.Haskell.TH.Quote
import Data.Data
import qualified System.IO as SIO

data Pieces =
    SubSite
        { ssType :: Type
        , ssParse :: Exp
        , ssRender :: Exp
        , ssDispatch :: Exp
        , ssToMasterArg :: Exp
        , ssPieces :: [Piece]
        }
  | Simple [Piece] [String] -- ^ methods
    deriving Show
type THResource = (String, Pieces)

createRoutes :: [THResource] -> Q [Con]
createRoutes res =
    return $ map go res
  where
    go (n, SubSite{ssType = s, ssPieces = pieces}) =
        NormalC (mkName n) $ mapMaybe go' pieces ++ [(NotStrict, s)]
    go (n, Simple pieces _) = NormalC (mkName n) $ mapMaybe go' pieces
    go' (SinglePiece x) = Just (NotStrict, ConT $ mkName x)
    go' (MultiPiece x) = Just (NotStrict, ConT $ mkName x)
    go' (StaticPiece _) = Nothing

-- | Generates the set of clauses necesary to parse the given 'Resource's. See 'quasiParse'.
createParse :: [THResource] -> Q [Clause]
createParse res = do
    final' <- final
    clauses <- mapM go res
    return $ if areResourcesComplete res
                then clauses
                else clauses ++ [final']
  where
    cons x y = ConP (mkName ":") [x, y]
    go (constr, SubSite{ssParse = p, ssPieces = ps}) = do
        ri <- [|Right|]
        be <- [|ape|]
        (pat', parse) <- mkPat' be ps $ ri `AppE` ConE (mkName constr)
        
        x <- newName "x"
        let pat = init pat' ++ [VarP x]

        --let pat = foldr (\a b -> cons [LitP (StringL a), b]) (VarP x) pieces
        let eitherSub = p `AppE` VarE x
        let bod = be `AppE` parse `AppE` eitherSub
        --let bod = fmape' `AppE` ConE (mkName constr) `AppE` eitherSub
        return $ Clause [foldr1 cons pat] (NormalB bod) []
    go (n, Simple ps _) = do
        ri <- [|Right|]
        be <- [|ape|]
        (pat, parse) <- mkPat' be ps $ ri `AppE` ConE (mkName n)
        return $ Clause [foldr1 cons pat] (NormalB parse) []
    final = do
        no <- [|Left "Invalid URL"|]
        return $ Clause [WildP] (NormalB no) []
    mkPat' :: Exp -> [Piece] -> Exp -> Q ([Pat], Exp)
    mkPat' be [MultiPiece s] parse = do
        v <- newName $ "var" ++ s
        fmp <- [|fromMultiPiece|]
        let parse' = InfixE (Just parse) be $ Just $ fmp `AppE` VarE v
        return ([VarP v], parse')
    mkPat' _ (MultiPiece _:_) _parse = error "MultiPiece must be last"
    mkPat' be (StaticPiece s:rest) parse = do
        (x, parse') <- mkPat' be rest parse
        let sp = LitP $ StringL s
        return (sp : x, parse')
    mkPat' be (SinglePiece s:rest) parse = do
        fsp <- [|fromSinglePiece|]
        v <- newName $ "var" ++ s
        let parse' = InfixE (Just parse) be $ Just $ fsp `AppE` VarE v
        (x, parse'') <- mkPat' be rest parse'
        return (VarP v : x, parse'')
    mkPat' _ [] parse = return ([ListP []], parse)

-- | 'ap' for 'Either'
ape :: Either String (a -> b) -> Either String a -> Either String b
ape (Left e) _ = Left e
ape (Right _) (Left e) = Left e
ape (Right f) (Right a) = Right $ f a

-- | Generates the set of clauses necesary to render the given 'Resource's. See
-- 'quasiRender'.
createRender :: [THResource] -> Q [Clause]
createRender = mapM go
  where
    go (n, Simple ps _) = do
        let ps' = zip [1..] ps
        let pat = ConP (mkName n) $ mapMaybe go' ps'
        bod <- mkBod ps'
        return $ Clause [pat] (NormalB $ TupE [bod, ListE []]) []
    go (n, SubSite{ssRender = r, ssPieces = pieces}) = do
        cons' <- [|\a (b, c) -> (a ++ b, c)|]
        let cons a b = cons' `AppE` a `AppE` b
        x <- newName "x"
        let r' = r `AppE` VarE x
        let pieces' = zip [1..] pieces
        let pat = ConP (mkName n) $ mapMaybe go' pieces' ++ [VarP x]
        bod <- mkBod pieces'
        return $ Clause [pat] (NormalB $ cons bod r') []
    go' (_, StaticPiece _) = Nothing
    go' (i, _) = Just $ VarP $ mkName $ "var" ++ show (i :: Int)
    mkBod :: (Show t) => [(t, Piece)] -> Q Exp
    mkBod [] = lift ([] :: [String])
    mkBod ((_, StaticPiece x):xs) = do
        x' <- lift x
        pack <- [|Data.Text.pack|]
        xs' <- mkBod xs
        return $ ConE (mkName ":") `AppE` (pack `AppE` x') `AppE` xs'
    mkBod ((i, SinglePiece _):xs) = do
        let x' = VarE $ mkName $ "var" ++ show i
        tsp <- [|toSinglePiece|]
        let x'' = tsp `AppE` x'
        xs' <- mkBod xs
        return $ ConE (mkName ":") `AppE` x'' `AppE` xs'
    mkBod ((i, MultiPiece _):_) = do
        let x' = VarE $ mkName $ "var" ++ show i
        tmp <- [|toMultiPiece|]
        return $ tmp `AppE` x'

-- | Whether the set of resources cover all possible URLs.
areResourcesComplete :: [THResource] -> Bool
areResourcesComplete res =
    let (slurps, noSlurps) = partitionEithers $ mapMaybe go res
     in case slurps of
            [] -> False
            _ -> let minSlurp = minimum slurps
                  in helper minSlurp $ reverse $ sort noSlurps
  where
    go :: THResource -> Maybe (Either Int Int)
    go (_, Simple ps _) =
        case reverse ps of
            [] -> Just $ Right 0
            (MultiPiece _:rest) -> go' Left rest
            x -> go' Right x
    go (n, SubSite{ssPieces = ps}) =
        go (n, Simple (ps ++ [MultiPiece ""]) [])
    go' b x = if all isSingle x then Just (b $ length x) else Nothing
    helper 0 _ = True
    helper _ [] = False
    helper m (i:is)
        | i >= m = helper m is
        | i + 1 == m = helper i is
        | otherwise = False
    isSingle (SinglePiece _) = True
    isSingle _ = False

notStatic :: Piece -> Bool
notStatic StaticPiece{} = False
notStatic _ = True

createDispatch :: Exp -- ^ modify a master handler
               -> Exp -- ^ convert a subsite handler to a master handler
               -> [THResource]
               -> Q [Clause]
createDispatch modMaster toMaster = mapM go
  where
    go :: (String, Pieces) -> Q Clause
    go (n, Simple ps methods) = do
        meth <- newName "method"
        xs <- mapM newName $ replicate (length $ filter notStatic ps) "x"
        let pat = [ ConP (mkName n) $ map VarP xs
                  , if null methods then WildP else VarP meth
                  ]
        bod <- go' n meth xs methods
        return $ Clause pat (NormalB bod) []
    go (n, SubSite{ssDispatch = d, ssToMasterArg = tma, ssPieces = ps}) = do
        meth <- newName "method"
        x <- newName "x"
        xs <- mapM newName $ replicate (length $ filter notStatic ps) "x"
        let pat = [ConP (mkName n) $ map VarP xs ++ [VarP x], VarP meth]
        let bod = d `AppE` VarE x `AppE` VarE meth
        fmap' <- [|fmap|]
        let routeToMaster = foldl AppE (ConE (mkName n)) $ map VarE xs
            tma' = foldl AppE tma $ map VarE xs
        let toMaster' = toMaster `AppE` routeToMaster `AppE` tma' `AppE` VarE x
        let bod' = InfixE (Just toMaster') fmap' (Just bod)
        let bod'' = InfixE (Just modMaster) fmap' (Just bod')
        return $ Clause pat (NormalB bod'') []
    go' n _ xs [] = do
        jus <- [|Just|]
        let bod = foldl AppE (VarE $ mkName $ "handle" ++ n) $ map VarE xs
        return $ jus `AppE` (modMaster `AppE` bod)
    go' n meth xs methods = do
        noth <- [|Nothing|]
        j <- [|Just|]
        let noMatch = Match WildP (NormalB noth) []
        return $ CaseE (VarE meth) $ map (go'' n xs j) methods ++ [noMatch]
    go'' n xs j method =
        let pat = LitP $ StringL method
            func = map toLower method ++ n
            bod = foldl AppE (VarE $ mkName func) $ map VarE xs
         in Match pat (NormalB $ j `AppE` (modMaster `AppE` bod)) []

-- | A quasi-quoter to parse a string into a list of 'Resource's. Checks for
-- overlapping routes, failing if present; use 'parseRoutesNoCheck' to skip the
-- checking. See documentation site for details on syntax.
parseRoutes :: QuasiQuoter
parseRoutes = QuasiQuoter
    { quoteExp = x
    , quotePat = y
    }
  where
    x s = do
        let res = resourcesFromString s
        case findOverlaps res of
            [] -> lift res
            z -> error $ "Overlapping routes: " ++ unlines (map show z)
    y = dataToPatQ (const Nothing) . resourcesFromString

parseRoutesFile :: FilePath -> Q Exp
parseRoutesFile fp = do
    s <- qRunIO $ readUtf8File fp
    quoteExp parseRoutes s

parseRoutesFileNoCheck :: FilePath -> Q Exp
parseRoutesFileNoCheck fp = do
    s <- qRunIO $ readUtf8File fp
    quoteExp parseRoutesNoCheck s

readUtf8File :: FilePath -> IO String
readUtf8File fp = do
    h <- SIO.openFile fp SIO.ReadMode
    SIO.hSetEncoding h SIO.utf8_bom
    SIO.hGetContents h

-- | Same as 'parseRoutes', but performs no overlap checking.
parseRoutesNoCheck :: QuasiQuoter
parseRoutesNoCheck = QuasiQuoter
    { quoteExp = x
    , quotePat = y
    }
  where
    x = lift . resourcesFromString
    y = dataToPatQ (const Nothing) . resourcesFromString

instance Lift Resource where
    lift (Resource s ps h) = do
        r <- [|Resource|]
        s' <- lift s
        ps' <- lift ps
        h' <- lift h
        return $ r `AppE` s' `AppE` ps' `AppE` h'

-- | A single resource pattern.
--
-- First argument is the name of the constructor, second is the URL pattern to
-- match, third is how to dispatch.
data Resource = Resource String [Piece] [String]
    deriving (Read, Show, Eq, Data, Typeable)

-- | A single piece of a URL, delimited by slashes.
--
-- In the case of StaticPiece, the argument is the value of the piece; for the
-- other constructors, it is the name of the parameter represented by this
-- piece. That value is not used here, but may be useful elsewhere.
data Piece = StaticPiece String
           | SinglePiece String
           | MultiPiece String
    deriving (Read, Show, Eq, Data, Typeable)

instance Lift Piece where
    lift (StaticPiece s) = do
        c <- [|StaticPiece|]
        s' <- lift s
        return $ c `AppE` s'
    lift (SinglePiece s) = do
        c <- [|SinglePiece|]
        s' <- lift s
        return $ c `AppE` s'
    lift (MultiPiece s) = do
        c <- [|MultiPiece|]
        s' <- lift s
        return $ c `AppE` s'

-- | Convert a multi-line string to a set of resources. See documentation for
-- the format of this string. This is a partial function which calls 'error' on
-- invalid input.
resourcesFromString :: String -> [Resource]
resourcesFromString =
    mapMaybe go . lines
  where
    go s =
        case takeWhile (/= "--") $ words s of
            (pattern:constr:rest) ->
                let pieces = piecesFromString $ drop1Slash pattern
                 in Just $ Resource constr pieces rest
            [] -> Nothing
            _ -> error $ "Invalid resource line: " ++ s

drop1Slash :: String -> String
drop1Slash ('/':x) = x
drop1Slash x = x

piecesFromString :: String -> [Piece]
piecesFromString "" = []
piecesFromString x =
    let (y, z) = break (== '/') x
     in pieceFromString y : piecesFromString (drop1Slash z)

pieceFromString :: String -> Piece
pieceFromString ('#':x) = SinglePiece x
pieceFromString ('*':x) = MultiPiece x
pieceFromString x = StaticPiece x

findOverlaps :: [Resource] -> [(Resource, Resource)]
findOverlaps = gos . map justPieces
  where
    justPieces r@(Resource _ ps _) = (ps, r)
    gos [] = []
    gos (x:xs) = mapMaybe (go x) xs ++ gos xs
    go (StaticPiece x:xs, xr) (StaticPiece y:ys, yr)
        | x == y = go (xs, xr) (ys, yr)
        | otherwise = Nothing
    go (MultiPiece _:_, xr) (_, yr) = Just (xr, yr)
    go (_, xr) (MultiPiece _:_, yr) = Just (xr, yr)
    go ([], xr) ([], yr) = Just (xr, yr)
    go ([], _) (_, _) = Nothing
    go (_, _) ([], _) = Nothing
    go (_:xs, xr) (_:ys, yr) = go (xs, xr) (ys, yr)
