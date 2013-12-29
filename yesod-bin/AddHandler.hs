module AddHandler (addHandler) where

import Prelude hiding (readFile)
import System.IO (hFlush, stdout)
import Data.Char (isLower, toLower, isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getDirectoryContents)

-- strict readFile
readFile :: FilePath -> IO String
readFile = fmap T.unpack . TIO.readFile

addHandler :: IO ()
addHandler = do
    allFiles <- getDirectoryContents "."
    cabal <-
        case filter (".cabal" `isSuffixOf`) allFiles of
            [x] -> return x
            [] -> error "No cabal file found"
            _ -> error "Too many cabal files found"

    putStr "Name of route (without trailing R): "
    hFlush stdout
    name <- getLine
    case name of
        [] -> error "Please provide a name"
        c:_
            | isLower c -> error "Name must start with an upper case letter"
            | otherwise -> return ()
    putStr "Enter route pattern (ex: /entry/#EntryId): "
    hFlush stdout
    pattern <- getLine
    putStr "Enter space-separated list of methods (ex: GET POST): "
    hFlush stdout
    methods <- getLine

    let modify fp f = readFile fp >>= writeFile fp . f

    modify "Application.hs" $ fixApp name
    modify cabal $ fixCabal name
    modify "config/routes" $ fixRoutes name pattern methods
    writeFile ("Handler/" ++ name ++ ".hs") $ mkHandler name pattern methods

fixApp :: String -> String -> String
fixApp name =
    unlines . reverse . go . reverse . lines
  where
    l = "import Handler." ++ name

    go [] = [l]
    go (x:xs)
        | "import Handler." `isPrefixOf` x = l : x : xs
        | otherwise = x : go xs

fixCabal :: String -> String -> String
fixCabal name =
    unlines . reverse . go . reverse . lines
  where
    l = "import Handler." ++ name

    go [] = [l]
    go (x:xs)
        | "Handler." `isPrefixOf` x' = (spaces ++ "Handler." ++ name) : x : xs
        | otherwise = x : go xs
      where
        (spaces, x') = span isSpace x

fixRoutes :: String -> String -> String -> String -> String
fixRoutes name pattern methods =
    (++ l)
  where
    l = concat
        [ pattern
        , " "
        , name
        , "R "
        , methods
        , "\n"
        ]

mkHandler :: String -> String -> String -> String
mkHandler name pattern methods = unlines
    $ ("module Handler." ++ name ++ " where")
    : ""
    : "import Import"
    : concatMap go (words methods)
  where
    go method =
        [ ""
        , concat $ func : " :: " : map toArrow types ++ ["Handler Html"]
        , concat
            [ func
            , " = error \"Not yet implemented: "
            , func
            , "\""
            ]
        ]
      where
        func = concat [map toLower method, name, "R"]

    types = getTypes pattern

    toArrow t = concat [t, " -> "]

    getTypes "" = []
    getTypes ('/':rest) = getTypes rest
    getTypes (c:rest) | c `elem` "#*" =
        typ : getTypes rest'
      where
        (typ, rest') = break (== '/') rest
    getTypes rest = getTypes $ dropWhile (/= '/') rest
