{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module EmbedTestGenerator (testGen) where

import Data.Default
import Network.Mime (MimeType)
import Yesod.EmbeddedStatic.Types
import Yesod.EmbeddedStatic.Generators (pathToName)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as BL

e1, e2, e3, e4 :: Entry

-- Basic entry
e1 = def
        { ebHaskellName = Just $ pathToName "e1"
        , ebLocation = "e1"
        , ebMimeType = "text/plain"
        , ebProductionContent = return $ TL.encodeUtf8 "e1 production"
        , ebDevelReload = [| return $ TL.encodeUtf8 $ TL.pack "e1 devel" |]
        , ebDevelExtraFiles = Nothing
        }

-- Test simulated directory in location
e2 = def
        { ebHaskellName = Just $ pathToName "e2"
        , ebLocation = "dir/e2"
        , ebMimeType = "abcdef"
        , ebProductionContent = return $ TL.encodeUtf8 "e2 production"
        , ebDevelReload = [| return $ TL.encodeUtf8 $ TL.pack "e2 devel" |]
        , ebDevelExtraFiles = Nothing
        }

-- Test empty haskell name
e3 = def
        { ebHaskellName = Nothing
        , ebLocation = "xxxx/e3"
        , ebMimeType = "yyy"
        , ebProductionContent = return $ TL.encodeUtf8 "e3 production"
        , ebDevelReload = [| return $ TL.encodeUtf8 $ TL.pack "e3 devel" |]
        , ebDevelExtraFiles = Nothing
        }

devExtra :: [T.Text] -> IO (Maybe (MimeType, BL.ByteString))
devExtra ["dev1"] = return $ Just ("mime", "dev1 content")
devExtra ["dir", "dev2"] = return $ Just ("mime2", "dev2 content")
devExtra _ = return Nothing

-- Entry with devel extra files
e4 = def
        { ebHaskellName = Just $ pathToName "e4"
        , ebLocation = "e4"
        , ebMimeType = "text/plain"
        , ebProductionContent = return $ TL.encodeUtf8 "e4 production"
        , ebDevelReload = [| return $ TL.encodeUtf8 $ TL.pack "e4 devel" |]
        , ebDevelExtraFiles = Just [| devExtra |]
        }

testGen :: Generator
testGen = return [e1, e2, e3, e4]
