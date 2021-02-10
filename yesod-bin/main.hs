{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards             #-}
module Main (main) where

import           Data.Monoid
import           Data.Version           (showVersion)
import           Options.Applicative
import           System.Exit            (exitFailure)

import           AddHandler             (addHandler)
import           Devel                  (DevelOpts (..), devel, develSignal)
import           Keter                  (keter)
import           Options                (injectDefaults)
import qualified Paths_yesod_bin

import           HsFile                 (mkHsFile)

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
             | Devel { develSuccessHook :: Maybe String
                     , develExtraArgs   :: [String]
                     , develPort        :: Int
                     , develTlsPort     :: Int
                     , proxyTimeout     :: Int
                     , noReverseProxy   :: Bool
                     , develHost        :: Maybe String
                     , cert             :: Maybe (FilePath, FilePath)
                     }
             | DevelSignal
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
         , ("yesod.build.extracabalarg" , \o args -> o { optCommand =
                case optCommand o of
                    b@Build{} -> b { buildExtraArgs = args }
                    c -> c
                })
         ] optParser'
  case optCommand o of
    Init _          -> initErrorMsg
    HsFiles         -> mkHsFile
    Configure       -> cabalErrorMsg
    Build _         -> cabalErrorMsg
    Touch           -> cabalErrorMsg
    Keter{..}       -> keter (cabalCommand o) _keterNoRebuild _keterNoCopyTo _keterBuildArgs
    Version         -> putStrLn ("yesod-bin version: " ++ showVersion Paths_yesod_bin.version)
    AddHandler{..}  -> addHandler addHandlerRoute addHandlerPattern addHandlerMethods
    Test            -> cabalErrorMsg
    Devel{..}       -> devel DevelOpts
                             { verbose      = optVerbose o
                             , successHook  = develSuccessHook
                             , develPort    = develPort
                             , develTlsPort = develTlsPort
                             , proxyTimeout = proxyTimeout
                             , useReverseProxy = not noReverseProxy
                             , develHost    = develHost
                             , cert         = cert
                             } develExtraArgs
    DevelSignal     -> develSignal
  where
    initErrorMsg = do
        mapM_ putStrLn
            [ "The init command has been removed."
            , "Please use 'stack new <project name> <template>' instead where the"
            , "available templates can be found by running 'stack templates'. For"
            , "a Yesod based application you should probably choose one of the"
            , "pre-canned Yesod templates."
            ]
        exitFailure

    cabalErrorMsg = do
        mapM_ putStrLn
            [ "The configure, build, touch, and test commands have been removed."
            , "Please use 'stack' for building your project."
            ]
        exitFailure

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
                            (progDesc "DEPRECATED"))
                      <> command "build"     (info (helper <*> (Build <$> extraCabalArgs))
                            (progDesc "DEPRECATED"))
                      <> command "touch"     (info (pure Touch)
                            (progDesc "DEPRECATED"))
                      <> command "devel"     (info (helper <*> develOptions)
                            (progDesc "Run project with the devel server"))
                      <> command "devel-signal"     (info (helper <*> pure DevelSignal)
                            (progDesc "Used internally by the devel command"))
                      <> command "test"      (info (pure Test)
                            (progDesc "DEPRECATED"))
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

develOptions :: Parser Command
develOptions = Devel <$> optStr ( long "success-hook" <> short 's' <> metavar "COMMAND"
                            <> help "Run COMMAND after rebuild succeeds")
                     <*> extraStackArgs
                     <*> option auto ( long "port" <> short 'p' <> value 3000 <> metavar "N"
                            <> help "Devel server listening port" )
                     <*> option auto ( long "tls-port" <> short 'q' <> value 3443 <> metavar "N"
                            <> help "Devel server listening port (tls)" )
                     <*> option auto ( long "proxy-timeout" <> short 'x' <> value 0 <> metavar "N"
                            <> help "Devel server timeout before returning 'not ready' message (in seconds, 0 for none)" )
                     <*> switch ( long "disable-reverse-proxy" <> short 'n'
                            <> help "Disable reverse proxy" )
                     <*> optStr (long "host" <> metavar "HOST"
                            <> help "Host interface to bind to; IP address, '*' for all interfaces, '*4' for IP4, '*6' for IP6")
                     <*> optional ( (,)
                            <$> strOption (long "cert" <> metavar "CERT"
                                   <> help "Path to TLS certificate file, requires that --key is also defined")
                            <*> strOption (long "key" <> metavar "KEY"
                                   <> help "Path to TLS key file, requires that --cert is also defined") )

extraStackArgs :: Parser [String]
extraStackArgs = many (strOption ( long "extra-stack-arg" <> short 'e' <> metavar "ARG"
                                   <> help "pass extra argument ARG to stack")
                      )

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
