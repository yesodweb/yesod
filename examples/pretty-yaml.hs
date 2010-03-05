{-# LANGUAGE QuasiQuotes #-}
import Yesod
import Data.Object.Yaml
import Network.Wai.Handler.SimpleServer
import Web.Encodings
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

data PY = PY TemplateGroup
instance YesodTemplate PY where
    getTemplateGroup (PY tg) = tg
    defaultTemplateAttribs _ _ = return
instance Yesod PY where
    resources = [$mkResources|
/:
    GET: homepageH
    POST: showYamlH
|]

homepageH :: Handler PY RepHtml
homepageH = templateHtml "pretty-yaml" return

showYamlH :: Handler PY RepHtmlJson
showYamlH = do
    rr <- getRequest
    (_, files) <- liftIO $ reqRequestBody rr
    fi <- case lookup "yaml" files of
            Nothing -> invalidArgs [("yaml", "Missing input")]
            Just x -> return x
    to <- liftIO $ decode $ B.concat $ L.toChunks $ fileContent fi
    let ho' = fmap Text to
    templateHtmlJson "pretty-yaml" ho' $ \ho ->
        return . setHtmlAttrib "yaml" (Scalar $ cs ho :: HtmlObject)

main :: IO ()
main = do
    putStrLn "Running..."
    loadTemplateGroup "examples" >>= toWaiApp . PY >>= run 3000
