{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import qualified Data.ByteString.Char8 as S8
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Application.Static
import Web.Routes

main = run 3000 $ debug app

app req =
    case decodePathInfo $ S8.unpack $ pathInfo req of
        "static":"foo":rest -> staticAppPieces StaticSettings
            { ssFolder = ".."
            , ssIndices = []
            , ssListing = Just defaultListing
            , ssGetMimeType = return . defaultMimeTypeByExt
            } rest req
        _ -> return $ responseLBS status404 [("Content-Type", "text/plain")] "Not found"
