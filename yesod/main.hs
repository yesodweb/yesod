{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad          (unless)
import           Data.Monoid
import           Data.Version           (showVersion)
import           Options.Applicative
import           System.Exit            (ExitCode (ExitSuccess), exitWith)
import           System.Process         (rawSystem)

import           Yesod.Core             (yesodVersion)

import           AddHandler             (addHandler)
import           Devel                  (DevelOpts (..), devel)
import           Keter                  (keter)
import           Options                (injectDefaults)
import qualified Paths_yesod
import           Scaffolding.Scaffolder

import           Options.Applicative.Builder.Internal (Mod, OptionFields)

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

data Command = Init
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
                     , _proxyTimeout     :: Int
                     }
             | Test
             | AddHandler
             | Keter { _keterNoRebuild :: Bool }
             | Version
  deriving (Show, Eq)

cabalCommand :: Options -> String
cabalCommand mopt
  | optCabalPgm mopt == CabalDev = "cabal-dev"
  | otherwise                    = "cabal"


main :: IO ()
main = do
  o <- execParser =<< injectDefaults "yesod" [ ("yesod.devel.extracabalarg" , \o args -> o { optCommand =
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
  let cabal xs = rawSystem' (cabalCommand o) xs
  case optCommand o of
    Init                    -> scaffold
    Configure               -> cabal ["configure"]
    Build es                -> touch' >> cabal ("build":es)
    Touch                   -> touch'
    Devel da s f r b _ig es p t -> devel (DevelOpts (optCabalPgm o == CabalDev) da (optVerbose o) r s f b p t) es
    Keter noRebuild         -> keter (cabalCommand o) noRebuild
    Version                 -> do putStrLn ("yesod-core version:" ++ yesodVersion)
                                  putStrLn ("yesod version:" ++ showVersion Paths_yesod.version)
    AddHandler              -> addHandler
    Test                    -> do touch'
                                  cabal ["configure", "--enable-tests", "-flibrary-only"]
                                  cabal ["build"]
                                  cabal ["test"]

optParser' :: ParserInfo Options
optParser' = info (helper <*> optParser) ( fullDesc <> header "Yesod Web Framework command line utility" )

optParser :: Parser Options
optParser = Options
        <$> flag Cabal CabalDev ( long "dev"     <> short 'd' <> help "use cabal-dev" )
        <*> switch              ( long "verbose" <> short 'v' <> help "More verbose output" )
        <*> subparser ( command "init"      (info (pure Init)
                            (progDesc "Scaffold a new site"))
                      <> command "configure" (info (pure Configure)
                            (progDesc "Configure a project for building"))
                      <> command "build"     (info (Build <$> extraCabalArgs)
                            (progDesc $ "Build project (performs TH dependency analysis)" ++ windowsWarning))
                      <> command "touch"     (info (pure Touch)
                            (progDesc $ "Touch any files with altered TH dependencies but do not build" ++ windowsWarning))
                      <> command "devel"     (info develOptions
                            (progDesc "Run project with the devel server"))
                      <> command "test"      (info (pure Test)
                            (progDesc "Build and run the integration tests"))
                      <> command "add-handler" (info (pure AddHandler)
                            (progDesc "Add a new handler and module to the project"))
                      <> command "keter"       (info keterOptions
                            (progDesc "Build a keter bundle"))
                      <> command "version"     (info (pure Version)
                            (progDesc "Print the version of Yesod"))
                      )

keterOptions :: Parser Command
keterOptions = Keter <$> switch ( long "nobuild" <> short 'n' <> help "Skip rebuilding" )

develOptions :: Parser Command
develOptions = Devel <$> switch ( long "disable-api"  <> short 'd'
                            <> help "Disable fast GHC API rebuilding")
                     <*> optStr ( long "success-hook" <> short 's' <> metavar "COMMAND"
                            <> help "Run COMMAND after rebuild succeeds")
                     <*> optStr ( long "failure-hook" <> short 'f' <> metavar "COMMAND"
                            <> help "Run COMMAND when rebuild fails")
                     <*> option ( long "event-timeout" <> short 't' <> value 1 <> metavar "N"
                            <> help "Force rescan of files every N seconds" )
                     <*> optStr ( long "builddir" <> short 'b'
                            <> help "Set custom cabal build directory, default `dist'")
                     <*> many ( strOption ( long "ignore" <> short 'i' <> metavar "DIR"
                                   <> help "ignore file changes in DIR" )
                              )
                     <*> extraCabalArgs
                     <*> option ( long "port" <> short 'p' <> value 3000 <> metavar "N"
                            <> help "Devel server listening port" )
                     <*> option ( long "proxy-timeout" <> short 'x' <> value 10 <> metavar "N"
                            <> help "Devel server timeout before returning 'not ready' message (in seconds)" )

extraCabalArgs :: Parser [String]
extraCabalArgs = many (strOption ( long "extra-cabal-arg" <> short 'e' <> metavar "ARG"
                                   <> help "pass extra argument ARG to cabal")
                      )

-- | Optional @String@ argument
optStr :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
optStr m =
    nullOption $ value Nothing <> reader (success . str)  <> m
  where
    success = Right

-- | Like @rawSystem@, but exits if it receives a non-success result.
rawSystem' :: String -> [String] -> IO ()
rawSystem' x y = do
    res <- rawSystem x y
    unless (res == ExitSuccess) $ exitWith res

