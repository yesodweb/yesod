{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-} -- QuasiQuoter

module Yesod.Routes.Parse
    ( parseRoutes
    , parseRoutesFile
    , parseRoutesNoCheck
    , parseRoutesFileNoCheck
    , parseType
    , parseTypeTree
    , TypeTree (..)
    , dropBracket
    , nameToType
    , isTvar
    ) where

import Language.Haskell.TH.Syntax
import Data.Char (isUpper, isLower, isSpace)
import Language.Haskell.TH.Quote
import qualified System.IO as SIO
import Yesod.Routes.TH
import Yesod.Routes.Overlap (findOverlapNames)
import Data.List (foldl', isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

-- | A quasi-quoter to parse a string into a list of 'Resource's. Checks for
-- overlapping routes, failing if present; use 'parseRoutesNoCheck' to skip the
-- checking. See documentation site for details on syntax.
parseRoutes :: QuasiQuoter
parseRoutes = QuasiQuoter { quoteExp = x }
  where
    x s = do
        let res = resourcesFromString s
        case findOverlapNames res of
            [] -> lift res
            z -> fail $ unlines $ "Overlapping routes: " : map show z

-- | Same as 'parseRoutes', but uses an external file instead of quasiquotation.
--
-- The recommended file extension is @.yesodroutes@.
parseRoutesFile :: FilePath -> Q Exp
parseRoutesFile = parseRoutesFileWith parseRoutes

-- | Same as 'parseRoutesNoCheck', but uses an external file instead of quasiquotation.
--
-- The recommended file extension is @.yesodroutes@.
parseRoutesFileNoCheck :: FilePath -> Q Exp
parseRoutesFileNoCheck = parseRoutesFileWith parseRoutesNoCheck

parseRoutesFileWith :: QuasiQuoter -> FilePath -> Q Exp
parseRoutesFileWith qq fp = do
    qAddDependentFile fp
    s <- qRunIO $ readUtf8File fp
    quoteExp qq s

readUtf8File :: FilePath -> IO String
readUtf8File fp = do
    h <- SIO.openFile fp SIO.ReadMode
    SIO.hSetEncoding h SIO.utf8_bom
    SIO.hGetContents h

-- | Same as 'parseRoutes', but performs no overlap checking.
parseRoutesNoCheck :: QuasiQuoter
parseRoutesNoCheck = QuasiQuoter
    { quoteExp = lift . resourcesFromString
    }

-- | Converts a multi-line string to a set of resources. See documentation for
-- the format of this string. This is a partial function which calls 'error' on
-- invalid input.
resourcesFromString :: String -> [ResourceTree String]
resourcesFromString =
    fst . parse 0 . filter (not . all (== ' ')) . foldr lineContinuations [] . lines . filter (/= '\r')
  where
    parse _ [] = ([], [])
    parse indent (thisLine:otherLines)
        | length spaces < indent = ([], thisLine : otherLines)
        | otherwise = (this others, remainder)
      where
        parseAttr ('!':x) = Just x
        parseAttr _ = Nothing

        stripColonLast =
            go id
          where
            go _ [] = Nothing
            go front [x]
                | null x = Nothing
                | last x == ':' = Just $ front [init x]
                | otherwise = Nothing
            go front (x:xs) = go (front . (x:)) xs

        spaces = takeWhile (== ' ') thisLine
        (others, remainder) = parse indent otherLines'
        (this, otherLines') =
            case takeWhile (not . isPrefixOf "--") $ splitSpaces thisLine of
                (pattern:rest0)
                    | Just (constr:rest) <- stripColonLast rest0
                    , Just attrs <- mapM parseAttr rest ->
                    let (children, otherLines'') = parse (length spaces + 1) otherLines
                        children' = addAttrs attrs children
                        (pieces, check) =
                            case piecesFromStringCheck pattern of
                                (p, Nothing, c) -> (p, c)
                                -- FIXME: Give better error message
                                _ -> error "Invalid resource line: bad overlap"
                     in ((ResourceParent constr check pieces children' :), otherLines'')
                (pattern:constr:rest) ->
                    let (pieces, mmulti, check) = piecesFromStringCheck pattern
                        (attrs, rest') = takeAttrs rest
                        disp = dispatchFromString rest' mmulti
                     in ((ResourceLeaf (Resource constr pieces disp attrs check):), otherLines)
                [] -> (id, otherLines)
                _ -> error $ "Invalid resource line: " ++ thisLine

-- | Splits a string by spaces, as long as the spaces are not enclosed by curly brackets (not recursive).
splitSpaces :: String -> [String]
splitSpaces "" = []
splitSpaces str =
    let (rest, piece) = parse $ dropWhile isSpace str in
    piece:(splitSpaces rest)

    where
        parse :: String -> ( String, String)
        parse ('{':s) = fmap ('{':) $ parseBracket s
        parse (c:s) | isSpace c = (s, [])
        parse (c:s) = fmap (c:) $ parse s
        parse "" = ("", "")

        parseBracket :: String -> ( String, String)
        parseBracket ('{':_) = error $ "Invalid resource line (nested curly bracket): " ++ str
        parseBracket ('}':s) = fmap ('}':) $ parse s
        parseBracket (c:s) = fmap (c:) $ parseBracket s
        parseBracket "" = error $ "Invalid resource line (unclosed curly bracket): " ++ str

piecesFromStringCheck :: String -> ([Piece String], Maybe String, Bool)
piecesFromStringCheck s0 =
    (pieces, mmulti, check)
  where
    (s1, check1) = stripBang s0
    (pieces', mmulti') = piecesFromString $ drop1Slash s1
    pieces = map snd pieces'
    mmulti = fmap snd mmulti'
    check = check1 && all fst pieces' && maybe True fst mmulti'

    stripBang ('!':rest) = (rest, False)
    stripBang x = (x, True)

addAttrs :: [String] -> [ResourceTree String] -> [ResourceTree String]
addAttrs attrs =
    map goTree
  where
    goTree (ResourceLeaf res) = ResourceLeaf (goRes res)
    goTree (ResourceParent w x y z) = ResourceParent w x y (map goTree z)

    goRes res =
        res { resourceAttrs = noDupes ++ resourceAttrs res }
      where
        usedKeys = Set.fromList $ map fst $ mapMaybe toPair $ resourceAttrs res
        used attr =
            case toPair attr of
                Nothing -> False
                Just (key, _) -> key `Set.member` usedKeys
        noDupes = filter (not . used) attrs

    toPair s =
        case break (== '=') s of
            (x, '=':y) -> Just (x, y)
            _ -> Nothing

-- | Take attributes out of the list and put them in the first slot in the
-- result tuple.
takeAttrs :: [String] -> ([String], [String])
takeAttrs =
    go id id
  where
    go x y [] = (x [], y [])
    go x y (('!':attr):rest) = go (x . (attr:)) y rest
    go x y (z:rest) = go x (y . (z:)) rest

dispatchFromString :: [String] -> Maybe String -> Dispatch String
dispatchFromString rest mmulti
    | null rest = Methods mmulti []
    | all (all isUpper) rest = Methods mmulti rest
dispatchFromString [subTyp, subFun] Nothing =
    Subsite subTyp subFun
dispatchFromString [_, _] Just{} =
    error "Subsites cannot have a multipiece"
dispatchFromString rest _ = error $ "Invalid list of methods: " ++ show rest

drop1Slash :: String -> String
drop1Slash ('/':x) = x
drop1Slash x = x

piecesFromString :: String -> ([(CheckOverlap, Piece String)], Maybe (CheckOverlap, String))
piecesFromString "" = ([], Nothing)
piecesFromString x =
    case (this, rest) of
        (Left typ, ([], Nothing)) -> ([], Just typ)
        (Left _, _) -> error "Multipiece must be last piece"
        (Right piece, (pieces, mtyp)) -> (piece:pieces, mtyp)
  where
    (y, z) = break (== '/') x
    this = pieceFromString y
    rest = piecesFromString $ drop 1 z

parseType :: String -> Type
parseType orig =
    maybe (error $ "Invalid type: " ++ show orig) ttToType $ parseTypeTree orig

parseTypeTree :: String -> Maybe TypeTree
parseTypeTree orig =
    toTypeTree pieces
  where
    pieces = filter (not . null) $ splitOn (\c -> c == '-' || c == ' ') $ addDashes orig
    addDashes [] = []
    addDashes (x:xs) =
        front $ addDashes xs
      where
        front rest
            | x `elem` "()[]" = '-' : x : '-' : rest
            | otherwise = x : rest
    splitOn c s =
        case y' of
            _:y -> x : splitOn c y
            [] -> [x]
      where
        (x, y') = break c s

data TypeTree = TTTerm String
              | TTApp TypeTree TypeTree
              | TTList TypeTree
    deriving (Show, Eq)

toTypeTree :: [String] -> Maybe TypeTree
toTypeTree orig = do
    (x, []) <- gos orig
    return x
  where
    go [] = Nothing
    go ("(":xs) = do
        (x, rest) <- gos xs
        case rest of
            ")":rest' -> Just (x, rest')
            _ -> Nothing
    go ("[":xs) = do
        (x, rest) <- gos xs
        case rest of
            "]":rest' -> Just (TTList x, rest')
            _ -> Nothing
    go (x:xs) = Just (TTTerm x, xs)

    gos xs1 = do
        (t, xs2) <- go xs1
        (ts, xs3) <- gos' id xs2
        Just (foldl' TTApp t ts, xs3)

    gos' front [] = Just (front [], [])
    gos' front (x:xs)
        | x `elem` words ") ]" = Just (front [], x:xs)
        | otherwise = do
            (t, xs') <- go $ x:xs
            gos' (front . (t:)) xs'

ttToType :: TypeTree -> Type
ttToType (TTTerm s) = fst $ nameToType s
ttToType (TTApp x y) = ttToType x `AppT` ttToType y
ttToType (TTList t) = ListT `AppT` ttToType t

nameToType :: String -> (Type, Name)
nameToType t = (, nm) $ if isTvar t
               then VarT nm
               else ConT nm
    where nm = mkName t

isTvar :: String -> Bool
isTvar (h:_) = isLower h
isTvar _     = False

pieceFromString :: String -> Either (CheckOverlap, String) (CheckOverlap, Piece String)
pieceFromString ('#':'!':x) = Right $ (False, Dynamic $ dropBracket x)
pieceFromString ('!':'#':x) = Right $ (False, Dynamic $ dropBracket x) -- https://github.com/yesodweb/yesod/issues/652
pieceFromString ('#':x) = Right $ (True, Dynamic $ dropBracket x)

pieceFromString ('*':'!':x) = Left (False, x)
pieceFromString ('+':'!':x) = Left (False, x)

pieceFromString ('!':'*':x) = Left (False, x)
pieceFromString ('!':'+':x) = Left (False, x)

pieceFromString ('*':x) = Left (True, x)
pieceFromString ('+':x) = Left (True, x)

pieceFromString ('!':x) = Right $ (False, Static x)
pieceFromString x = Right $ (True, Static x)

dropBracket :: String -> String
dropBracket str@('{':x) = case break (== '}') x of
    (s, "}") -> s
    _ -> error $ "Unclosed bracket ('{'): " ++ str
dropBracket x = x

-- | If this line ends with a backslash, concatenate it together with the next line.
--
-- @since 1.6.8
lineContinuations :: String -> [String] -> [String]
lineContinuations this [] = [this]
lineContinuations this below@(next:rest) = case unsnoc this of
    Just (this', '\\') -> (this' ++ next):rest
    _ -> this:below
  where unsnoc s = if null s then Nothing else Just (init s, last s)
