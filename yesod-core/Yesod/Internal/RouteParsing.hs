{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-} -- QuasiQuoter
module Yesod.Internal.RouteParsing
    ( parseRoutes
    , parseRoutesFile
    , parseRoutesNoCheck
    , parseRoutesFileNoCheck
    , RouteString
    , parseRouteString
    ) where

import Web.PathPieces
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.Either
import Data.List
import Data.Char (toLower, isUpper)
import qualified Data.Text
import Language.Haskell.TH.Quote
import Data.Data
import qualified System.IO as SIO
import Yesod.Routes.TH

-- | A quasi-quoter to parse a string into a list of 'Resource's. Checks for
-- overlapping routes, failing if present; use 'parseRoutesNoCheck' to skip the
-- checking. See documentation site for details on syntax.
parseRoutes :: QuasiQuoter
parseRoutes = QuasiQuoter
    { quoteExp = x
    }
  where
    x s = do
        let res = resourcesFromString s
        case findOverlaps res of
            [] -> liftParse s
            z -> error $ "Overlapping routes: " ++ unlines (map (unwords . map resourceName) z)

newtype RouteString = RouteString String

liftParse :: String -> Q Exp
liftParse s = [|RouteString s|]

parseRouteString :: RouteString -> [Resource]
parseRouteString (RouteString s) = resourcesFromString s

instance Lift RouteString where
    lift (RouteString s) = [|RouteString $(lift s)|]

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
    { quoteExp = liftParse
    }

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
                let (pieces, mmulti) = piecesFromString $ drop1Slash pattern
                    disp = dispatchFromString rest mmulti
                 in Just $ Resource constr pieces disp
            [] -> Nothing
            _ -> error $ "Invalid resource line: " ++ s

dispatchFromString :: [String] -> Maybe Type -> Dispatch
dispatchFromString rest mmulti
    | null rest = Methods mmulti []
    | all (all isUpper) rest = Methods mmulti rest
dispatchFromString [subTyp, subFun] Nothing =
    Subsite (parseType subTyp) subFun
dispatchFromString [subTyp, subFun] Just{} =
    error "Subsites cannot have a multipiece"
dispatchFromString rest _ = error $ "Invalid list of methods: " ++ show rest

drop1Slash :: String -> String
drop1Slash ('/':x) = x
drop1Slash x = x

piecesFromString :: String -> ([Piece], Maybe Type)
piecesFromString "" = ([], Nothing)
piecesFromString x =
    case (this, rest) of
        (Left typ, ([], Nothing)) -> ([], Just typ)
        (Left typ, _) -> error "Multipiece must be last piece"
        (Right piece, (pieces, mtyp)) -> (piece:pieces, mtyp)
  where
    (y, z) = break (== '/') x
    this = pieceFromString y
    rest = piecesFromString $ drop 1 z

parseType :: String -> Type
parseType = ConT . mkName -- FIXME handle more complicated stuff

pieceFromString :: String -> Either Type Piece
pieceFromString ('#':x) = Right $ Dynamic $ parseType x
pieceFromString ('*':x) = Left $ parseType x
pieceFromString x = Right $ Static x

-- n^2, should be a way to speed it up
findOverlaps :: [Resource] -> [[Resource]]
findOverlaps = go . map justPieces
  where
    justPieces :: Resource -> ([Piece], Resource)
    justPieces r@(Resource _ ps _) = (ps, r)

    go [] = []
    go (x:xs) = mapMaybe (mOverlap x) xs ++ go xs

    mOverlap :: ([Piece], Resource) -> ([Piece], Resource) ->
                Maybe [Resource]
    mOverlap _ _ = Nothing
                {- FIXME mOverlap
    mOverlap (Static x:xs, xr) (Static y:ys, yr)
        | x == y = mOverlap (xs, xr) (ys, yr)
        | otherwise = Nothing
    mOverlap (MultiPiece _:_, xr) (_, yr) = Just (xr, yr)
    mOverlap (_, xr) (MultiPiece _:_, yr) = Just (xr, yr)
    mOverlap ([], xr) ([], yr) = Just (xr, yr)
    mOverlap ([], _) (_, _) = Nothing
    mOverlap (_, _) ([], _) = Nothing
    mOverlap (_:xs, xr) (_:ys, yr) = mOverlap (xs, xr) (ys, yr)
    -}
