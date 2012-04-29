{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-} -- QuasiQuoter
module Yesod.Routes.Parse
    ( parseRoutes
    , parseRoutesFile
    , parseRoutesNoCheck
    , parseRoutesFileNoCheck
    , parseType
    , parseRoutePaths
    ) where

import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.Char (isUpper, isSpace)
import Language.Haskell.TH.Quote
import qualified System.IO as SIO
import Yesod.Routes.TH
import Yesod.Routes.Overlap (findOverlapNames)
import System.FilePath.Posix ((</>))

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
            z -> error $ "Overlapping routes: " ++ unlines (map show z)

parseRoutesFile :: FilePath -> Q Exp
parseRoutesFile = parseRoutesFileWith parseRoutes

parseRoutesFileNoCheck :: FilePath -> Q Exp
parseRoutesFileNoCheck = parseRoutesFileWith parseRoutesNoCheck

parseRoutesFileWith :: QuasiQuoter -> FilePath -> Q Exp
parseRoutesFileWith qq fp = do
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
    { quoteExp = lift . resourcesFromString }

-- | Convert a multi-line string to a set of route paths.
-- like normal route parsing, but there are just route paths, no route constructors or HTTP methods
-- This can be used as a DSL for generating route paths that
-- * closely matches your current routes file
-- * allows indentation to imply prefixes
--
-- This is a partial function which calls 'error' on invalid input.
parseRoutePaths :: String -> [String]
parseRoutePaths = parseRoutesFromString staticPageRoute
  where
    staticPageRoute :: String -> [String] -> Maybe String
    staticPageRoute r [] = Just $ stripEndSlash r
    staticPageRoute r rest = error $ "line starting with: " ++ r ++ "\ndid not expect: " ++ show rest
    stripEndSlash r = if last r == '/' then init r else r

-- | Convert a multi-line string to a set of resources. See documentation for
-- the format of this string. This is a partial function which calls 'error' on
-- invalid input.
resourcesFromString :: String -> [Resource String]
resourcesFromString =
    parseRoutesFromString resourceFromLine

resourceFromLine :: String -> [String] -> Maybe (Resource String)
resourceFromLine fullRoute (constr:rest) =
            let (pieces, mmulti) = piecesFromString $ fullRoute
                disp = dispatchFromString rest mmulti
             in Just $ Resource constr pieces disp
resourceFromLine _ [] = Nothing -- an indenter: there should be indented routes afterwards

  
-- | used by 'resourcesFromString' and 'staticPageRoutesFromString'
parseRoutesFromString :: (String  -- ^ route pattern
                      -> [String] -- ^ extra
                      -> Maybe a)
                      -> String -- ^ unparsed routes
                      -> [a]
parseRoutesFromString mkRoute =
    catMaybes . (parseLines $ error "first route cannot be indented") . lines
  where
    indents :: String -> Int
    indents = length . takeWhile isSpace

    parseLines noIndent (l:ls) =
        case takeWhile (/= "--") $ words l of
            (route:rest) ->
                let (newNoIndent, fullRoute) =
                        if indents l == 0
                          -- important: the check is done lazily
                          then (route, route)
                          else (noIndent, noIndent </> route)
                 in mkRoute (dropPreSlash fullRoute) rest : parseLines newNoIndent ls
            [] -> parseLines noIndent ls
    parseLines _ [] = []

dispatchFromString :: [String] -> Maybe String -> Dispatch String
dispatchFromString rest mmulti
    | null rest = Methods mmulti []
    | all (all isUpper) rest = Methods mmulti rest
dispatchFromString [subTyp, subFun] Nothing =
    Subsite subTyp subFun
dispatchFromString [_, _] Just{} =
    error "Subsites cannot have a multipiece"
dispatchFromString rest _ = error $ "Invalid list of methods: " ++ show rest

dropPreSlash :: String -> String
dropPreSlash ('/':x) = x
dropPreSlash x = x

piecesFromString :: String -> ([(CheckOverlap, Piece String)], Maybe String)
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
parseType = ConT . mkName -- FIXME handle more complicated stuff

pieceFromString :: String -> Either String (CheckOverlap, Piece String)
pieceFromString ('#':'!':x) = Right $ (False, Dynamic x)
pieceFromString ('#':x) = Right $ (True, Dynamic x)
pieceFromString ('*':x) = Left x
pieceFromString ('!':x) = Right $ (False, Static x)
pieceFromString x = Right $ (True, Static x)
