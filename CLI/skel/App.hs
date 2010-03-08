{-# LANGUAGE QuasiQuotes #-}
import Yesod
import Yesod.Helpers.Static
import qualified Data.Object.Yaml
import qualified Safe.Failure

data $Datatype$ = $Datatype$
    { settings :: Settings
    , templateGroup :: TemplateGroup
    }

data Settings = Settings
    { sApproot :: String
    , staticRoot :: String
    , staticDir :: String
    , templateDir :: String
    , portNumber :: Int
    }

settingsFile :: FilePath
settingsFile = "settings.yaml"

loadSettings :: IO Settings
loadSettings = do
    m <- Data.Object.Yaml.decodeFile settingsFile >>= fromMapping
    ar <- lookupScalar "approot" m
    sr <- lookupScalar "static-root" m
    sd <- lookupScalar "static-dir" m
    td <- lookupScalar "template-dir" m
    pn <- lookupScalar "port" m >>= Safe.Failure.read
    return \$ Settings ar sr sd td pn

load$Datatype$ :: IO $Datatype$
load$Datatype$ = do
    s <- loadSettings
    tg <- loadTemplateGroup \$ templateDir s
    return \$ $Datatype$ s tg

main :: IO ()
main = do
    datatype <- load$Datatype$
    app <- toWaiApp datatype
    basicHandler (portNumber \$ settings datatype) app

instance Yesod $Datatype$ where
    resources = [\$mkResources|
/:
    GET: homepageH
/static/*: serveStatic'
|]
    applyLayout = defaultApplyLayout

instance YesodApproot $Datatype$ where
    approot = sApproot . settings

instance YesodTemplate $Datatype$ where
    getTemplateGroup = templateGroup
    defaultTemplateAttribs y _ = return
        . setHtmlAttrib "approot" (approot y)
        . setHtmlAttrib "staticroot" (staticRoot \$ settings y)

homepageH :: Handler $Datatype$ RepHtml
homepageH = templateHtml "homepage" return

serveStatic' :: Method -> [String]
             -> Handler $Datatype$ [(ContentType, Content)]
serveStatic' method pieces = do
    y <- getYesod
    let sd = staticDir \$ settings y
    serveStatic (fileLookupDir sd) method pieces
