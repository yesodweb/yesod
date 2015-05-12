{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module HsFile (mkHsFile) where
import Text.ProjectTemplate (createTemplate)
import Data.Conduit 
    ( ($$), (=$), ConduitM, awaitForever, yield, Source )
import Data.Conduit.Filesystem (sourceDirectory)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)

mkHsFile :: IO ()
mkHsFile = runResourceT $ sourceDirectory "."
        $$ readIt
        =$ createTemplate 
        =$ awaitForever (liftIO . BS.putStr)

-- Reads a filepath from upstream and dumps a pair of (filepath, filecontents)
readIt :: ConduitM FilePath (FilePath, ResourceT IO BS.ByteString) (ResourceT IO) ()
readIt = CL.map $ \i -> (i, liftIO $ BS.readFile i)

