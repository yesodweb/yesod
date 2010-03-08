{-# LANGUAGE TemplateHaskell #-}
import Data.FileEmbed
import Text.StringTemplate
import Data.ByteString.Char8 (ByteString, unpack)
import System.Directory
import System.Environment
import System.IO
import Data.Char

skel :: [(FilePath, ByteString)]
skel = $(embedDir "CLI/skel")

yesodInit :: FilePath -> [(String, String)] -> IO ()
yesodInit topDir a = do
  mapM_ (\x -> createDirectoryIfMissing True $ topDir ++ x)
    ["static", "templates"]
  mapM_ go skel
   where
    go (fp, bs) = do
        let temp = newSTMP $ unpack bs
        writeFile (topDir ++ fp) $ toString $ setManyAttrib a temp

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["init"] -> yesodInit'
        _ -> usage

usage :: IO ()
usage = putStrLn "Currently, the only support operation is \"init\"."

prompt :: String -> (String -> Bool) -> IO String
prompt s t = do
    putStr s
    hFlush stdout
    x <- getLine
    if t x
        then return x
        else do
            putStrLn "That was not valid input."
            prompt s t

yesodInit' :: IO ()
yesodInit' = do
    putStrLn "Let's get started created a Yesod web application."
    dest <-
        prompt
            "In which directory would you like to put the application? "
            (not . null)
    dt <-
        prompt
            "Give a data type name (first letter capital): "
            (\x -> not (null x) && isUpper (head x))
    pr <- prompt
            "Name of project (cabal file): "
            (not . null)
    au <- prompt
            "Author (cabal file): "
            (not . null)
    em <- prompt
            "Author email (cabal file): "
            (not . null)
    ho <- prompt
            "Homepage (cabal file): "
            (not . null)
    yesodInit (dest ++ "/")
        [ ("Datatype", dt)
        , ("project", pr)
        , ("author", au)
        , ("email", em)
        , ("homepage", ho)
        ]
    renameFile (dest ++ "/webapp.cabal") (dest ++ "/" ++ pr ++ ".cabal")
    renameFile (dest ++ "/App.hs") (dest ++ "/" ++ dt ++ ".hs")
    putStrLn "Your project has been initialized."
