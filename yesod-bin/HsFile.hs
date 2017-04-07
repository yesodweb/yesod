{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module HsFile (mkHsFile) where
import Text.ProjectTemplate (createTemplate)
import Data.Conduit 
    ( ($$), (=$), awaitForever)
import Data.Conduit.Filesystem (sourceDirectory)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)

mkHsFile :: IO ()
mkHsFile = runResourceT $ sourceDirectory "."
        $$ readIt
        =$ createTemplate 
        =$ awaitForever (liftIO . BS.putStr)
  where
    -- Reads a filepath from upstream and dumps a pair of (filepath, filecontents)
    readIt = CL.map $ \i -> (fromString i, liftIO $ BS.readFile i)
