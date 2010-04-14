{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Data.Object.Yaml
import Network.Wai.Handler.SimpleServer
import Web.Encodings
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Object.String

data PY = PY

mkYesod "PY" [$parseRoutes|
/ Homepage GET POST
|]

instance Yesod PY where
    approot _ = "http://localhost:3000"

template :: Monad m => TempArgs url m -> Hamlet url m ()
template = [$hamlet|
!!!
%html
    %head
        %meta!charset=utf-8
        %title Pretty YAML
    %body
        %form!method=post!action=.!enctype=multipart/form-data
            File name:
            %input!type=file!name=yaml
            %input!type=submit
        $if hasYaml
            %div ^yaml^
|]

data TempArgs url m = TempArgs
    { hasYaml :: m Bool
    , yaml :: Hamlet url m ()
    }

getHomepage :: Handler PY RepHtml
getHomepage = hamletToRepHtml
            $ template $ TempArgs (return False) (return ())

--FIXMEpostHomepage :: Handler PY RepHtmlJson
postHomepage :: Handler PY RepHtml
postHomepage = do
    rr <- getRequest
    (_, files) <- liftIO $ reqRequestBody rr
    fi <- case lookup "yaml" files of
            Nothing -> invalidArgs [("yaml", "Missing input")]
            Just x -> return x
    so <- liftIO $ decode $ B.concat $ L.toChunks $ fileContent fi
    {-
    let ho' = fmap Text to
    templateHtmlJson "pretty-yaml" ho' $ \ho ->
        return . setHtmlAttrib "yaml" (Scalar $ cs ho :: HtmlObject)
    -}
    let ho = cs (so :: StringObject) :: HtmlObject
    hamletToRepHtml $ template $ TempArgs (return True) (cs ho)

main :: IO ()
main = do
    putStrLn "Running..."
    toWaiApp PY >>= run 3000
