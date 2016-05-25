{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards             #-}

import           Control.Monad          (unless)
import           Data.Monoid
import           Data.Version           (showVersion)
import           Options.Applicative
import           System.Environment     (getEnvironment)
import           System.Exit            (ExitCode (ExitSuccess), exitWith, exitFailure)
import           System.FilePath        (splitSearchPath)
import           System.Process         (rawSystem)

import           AddHandler             (addHandler)
import           Devel                  (DevelOpts (..), devel, DevelTermOpt(..))
import           Keter                  (keter)
import           Options                (injectDefaults)
import qualified Paths_yesod_bin
import           System.IO              (hPutStrLn, stderr)

import           HsFile                 (mkHsFile)
#ifndef WINDOWS
import           Build                  (touch)

touch' :: IO ()
touch' = touch

windowsWarning :: String
windowsWarning = ""
#else
touch' :: IO ()
touch'  = return ()

windowsWarning :: String
windowsWarning = " (does not work on Windows)"
#endif

data CabalPgm = Cabal | CabalDev deriving (Show, Eq)

data Options = Options
               { optCabalPgm :: CabalPgm
               , optVerbose  :: Bool
               , optCommand  :: Command
               }
  deriving (Show, Eq)

data Command = Init [String]
             | HsFiles
             | Configure
             | Build { buildExtraArgs   :: [String] }
             | Touch
             | Devel { _develDisableApi  :: Bool
                     , _develSuccessHook :: Maybe String
                     , _develFailHook    :: Maybe String
                     , _develRescan      :: Int
                     , _develBuildDir    :: Maybe String
                     , develIgnore       :: [String]
                     , develExtraArgs    :: [String]
                     , _develPort        :: Int
                     , _develTlsPort     :: Int
                     , _proxyTimeout     :: Int
                     , _noReverseProxy   :: Bool
                     , _interruptOnly    :: Bool
                     }
             | Test
             | AddHandler
                    { addHandlerRoute   :: Maybe String
                    , addHandlerPattern :: Maybe String
                    , addHandlerMethods :: [String]
                    }
             | Keter
                    { _keterNoRebuild :: Bool
                    , _keterNoCopyTo  :: Bool
                    , _keterBuildArgs :: [String]
                    }
             | Version
  deriving (Show, Eq)

cabalCommand :: Options -> String
cabalCommand mopt
  | optCabalPgm mopt == CabalDev = "cabal-dev"
  | otherwise                    = "cabal"


main :: IO ()
main = do
  o <- execParser =<< injectDefaults "yesod"
         [ ("yesod.devel.extracabalarg" , \o args -> o { optCommand =
                case optCommand o of
                    d@Devel{} -> d { develExtraArgs = args }
                    c -> c
                })
         , ("yesod.devel.ignore"        , \o args -> o { optCommand =
                case optCommand o of
                    d@Devel{} -> d { develIgnore = args }
                    c -> c
                })
         , ("yesod.build.extracabalarg" , \o args -> o { optCommand =
                case optCommand o of
                    b@Build{} -> b { buildExtraArgs = args }
                    c -> c
                })
         ] optParser'
  let cabal = rawSystem' (cabalCommand o)
  case optCommand o of
    Init _          -> initErrorMsg
    HsFiles         -> mkHsFile
    Configure       -> cabal ["configure"]
    Build es        -> touch' >> cabal ("build":es)
    Touch           -> touch'
    Keter{..}       -> keter (cabalCommand o) _keterNoRebuild _keterNoCopyTo _keterBuildArgs
    Version         -> putStrLn ("yesod-bin version: " ++ showVersion Paths_yesod_bin.version)
    AddHandler{..}  -> addHandler addHandlerRoute addHandlerPattern addHandlerMethods
    Test            -> cabalTest cabal
    Devel{..}       ->do
                       (configOpts, menv) <- handleGhcPackagePath
                       let develOpts = DevelOpts
                             { isCabalDev   = optCabalPgm o == CabalDev
                             , forceCabal   = _develDisableApi
                             , verbose      = optVerbose o
                             , eventTimeout = _develRescan
                             , successHook  = _develSuccessHook
                             , failHook     = _develFailHook
                             , buildDir     = _develBuildDir
                             , develPort    = _develPort
                             , develTlsPort = _develTlsPort
                             , proxyTimeout = _proxyTimeout
                             , useReverseProxy = not _noReverseProxy
                             , terminateWith = if _interruptOnly then TerminateOnlyInterrupt else TerminateOnEnter
                             , develConfigOpts = configOpts
                             , develEnv = menv
                             }
                       devel develOpts develExtraArgs
  where
    cabalTest cabal = do
        env <- getEnvironment
        case lookup "STACK_EXE" env of
            Nothing -> do
                touch'
                _ <- cabal ["configure", "--enable-tests", "-flibrary-only"]
                _ <- cabal ["build"]
                cabal ["test"]
            Just _ -> do
                hPutStrLn stderr "'yesod test' is no longer needed with Stack"
                hPutStrLn stderr "Instead, please just run 'stack test'"
                exitFailure

    initErrorMsg = do
        mapM_ putStrLn
            [ "The init command has been removed."
            , "Please use 'stack new <project name> <template>' instead where the"
            , "available templates can be found by running 'stack templates'. For"
            , "a Yesod based application you should probably choose one of the"
            , "pre-canned Yesod templates."
            ]
        exitFailure


handleGhcPackagePath :: IO ([String], Maybe [(String, String)])
handleGhcPackagePath = do
    env <- getEnvironment
    case lookup "GHC_PACKAGE_PATH" env of
        Nothing -> return ([], Nothing)
        Just gpp -> do
            let opts = "--package-db=clear"
                     : "--package-db=global"
                     : map ("--package-db=" ++)
                       (drop 1 $ reverse $ splitSearchPath gpp)
            return (opts, Just $ filter (\(x, _) -> x /= "GHC_PACKAGE_PATH") env)

optParser' :: ParserInfo Options
optParser' = info (helper <*> optParser) ( fullDesc <> header "Yesod Web Framework command line utility" )

optParser :: Parser Options
optParser = Options
        <$> flag Cabal CabalDev ( long "dev"     <> short 'd' <> help "use cabal-dev" )
        <*> switch              ( long "verbose" <> short 'v' <> help "More verbose output" )
        <*> subparser ( command "init"         (info (helper <*> initOptions)
                            (progDesc "Command no longer available, please use 'stack new'"))
                      <> command "hsfiles" (info (pure HsFiles)
                            (progDesc "Create a hsfiles file for the current folder"))
                      <> command "configure" (info (pure Configure)
                            (progDesc "Configure a project for building"))
                      <> command "build"     (info (helper <*> (Build <$> extraCabalArgs))
                            (progDesc $ "Build project (performs TH dependency analysis)" ++ windowsWarning))
                      <> command "touch"     (info (pure Touch)
                            (progDesc $ "Touch any files with altered TH dependencies but do not build" ++ windowsWarning))
                      <> command "devel"     (info (helper <*> develOptions)
                            (progDesc "Run project with the devel server"))
                      <> command "test"      (info (pure Test)
                            (progDesc "Build and run the integration tests"))
                      <> command "add-handler" (info (helper <*> addHandlerOptions)
                            (progDesc ("Add a new handler and module to the project."
                            ++ " Interactively asks for input if you do not specify arguments.")))
                      <> command "keter"       (info (helper <*> keterOptions)
                            (progDesc "Build a keter bundle"))
                      <> command "version"     (info (pure Version)
                            (progDesc "Print the version of Yesod"))
                      )

initOptions :: Parser Command
initOptions = Init <$> many (argument str mempty)

keterOptions :: Parser Command
keterOptions = Keter
    <$> switch ( long "nobuild" <> short 'n' <> help "Skip rebuilding" )
    <*> switch ( long "nocopyto" <> help "Ignore copy-to directive in keter config file" )
    <*> optStrToList ( long "build-args" <> help "Build arguments" )
  where
    optStrToList m = option (words <$> str) $ value [] <> m

defaultRescan :: Int
defaultRescan = 10

develOptions :: Parser Command
develOptions = Devel <$> switch ( long "disable-api"  <> short 'd'
                            <> help "Disable fast GHC API rebuilding")
                     <*> optStr ( long "success-hook" <> short 's' <> metavar "COMMAND"
                            <> help "Run COMMAND after rebuild succeeds")
                     <*> optStr ( long "failure-hook" <> short 'f' <> metavar "COMMAND"
                            <> help "Run COMMAND when rebuild fails")
                     <*> option auto ( long "event-timeout" <> short 't' <> value defaultRescan <> metavar "N"
                            <> help ("Force rescan of files every N seconds (default "
                                     ++ show defaultRescan
                                     ++ ", use -1 to rely on FSNotify alone)") )
                     <*> optStr ( long "builddir" <> short 'b'
                            <> help "Set custom cabal build directory, default `dist'")
                     <*> many ( strOption ( long "ignore" <> short 'i' <> metavar "DIR"
                                   <> help "ignore file changes in DIR" )
                              )
                     <*> extraCabalArgs
                     <*> option auto ( long "port" <> short 'p' <> value 3000 <> metavar "N"
                            <> help "Devel server listening port" )
                     <*> option auto ( long "tls-port" <> short 'q' <> value 3443 <> metavar "N"
                            <> help "Devel server listening port (tls)" )
                     <*> option auto ( long "proxy-timeout" <> short 'x' <> value 0 <> metavar "N"
                            <> help "Devel server timeout before returning 'not ready' message (in seconds, 0 for none)" )
                     <*> switch ( long "disable-reverse-proxy" <> short 'n'
                            <> help "Disable reverse proxy" )
                     <*> switch ( long "interrupt-only"  <> short 'c'
                            <> help "Disable exiting when enter is pressed")

extraCabalArgs :: Parser [String]
extraCabalArgs = many (strOption ( long "extra-cabal-arg" <> short 'e' <> metavar "ARG"
                                   <> help "pass extra argument ARG to cabal")
                      )

addHandlerOptions :: Parser Command
addHandlerOptions = AddHandler
    <$> optStr ( long "route" <> short 'r' <> metavar "ROUTE"
           <> help "Name of route (without trailing R). Required.")
    <*> optStr ( long "pattern" <> short 'p' <> metavar "PATTERN"
           <> help "Route pattern (ex: /entry/#EntryId). Defaults to \"\".")
    <*> many (strOption ( long "method" <> short 'm' <> metavar "METHOD"
                 <> help "Takes one method. Use this multiple times to add multiple methods. Defaults to none.")
             )

-- | Optional @String@ argument
optStr :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
optStr m = option (Just <$> str) $ value Nothing <> m

-- | Like @rawSystem@, but exits if it receives a non-success result.
rawSystem' :: String -> [String] -> IO ()
rawSystem' x y = do
    res <- rawSystem x y
    unless (res == ExitSuccess) $ exitWith res

