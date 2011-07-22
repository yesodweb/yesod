{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import Network.Wai.Application.Static
    ( StaticSettings (..), staticApp, defaultMimeType, defaultListing
    , defaultMimeTypes, mimeTypeByExt
    )
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import System.Console.CmdArgs
import Text.Printf (printf)
import System.Directory (canonicalizePath)
import Control.Monad (unless)
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.Debug
import Network.Wai.Middleware.Gzip
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as S8
import Control.Arrow (second)

data Args = Args
    { docroot :: FilePath
    , index :: [FilePath]
    , port :: Int
    , noindex :: Bool
    , quiet :: Bool
    , verbose :: Bool
    , mime :: [(String, String)]
    }
    deriving (Show, Data, Typeable)

defaultArgs = Args "." ["index.html", "index.htm"] 3000 False False False []

main :: IO ()
main = do
    Args {..} <- cmdArgs defaultArgs
    let mime' = map (second S8.pack) mime
    let mimeMap = Map.fromList mime' `Map.union` defaultMimeTypes
    docroot' <- canonicalizePath docroot
    args <- getArgs
    unless quiet $ printf "Serving directory %s on port %d with %s index files.\n" docroot' port (if noindex then "no" else show index)
    let middle = gzip False
               . (if verbose then debug else id)
               . autohead
    run port $ middle $ staticApp StaticSettings
        { ssFolder = docroot
        , ssIndices = if noindex then [] else index
        , ssListing = Just defaultListing
        , ssGetMimeType = return . mimeTypeByExt mimeMap defaultMimeType
        }
