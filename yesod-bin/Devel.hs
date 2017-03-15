{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
module Devel
    ( devel
    , develSignal
    , DevelOpts(..)
    ) where

import           Control.Applicative                   ((<|>))
import           Control.Concurrent                    (threadDelay)
import           Control.Concurrent.Async              (race_)
import           Control.Concurrent.STM
import qualified Control.Exception.Safe                as Ex
import           Control.Monad                         (forever, unless, void,
                                                        when)
import qualified Data.ByteString.Lazy                  as LB
import           Data.Conduit                          (($$), (=$))
import qualified Data.Conduit.Binary                   as CB
import qualified Data.Conduit.List                     as CL
import           Data.Default.Class                    (def)
import           Data.FileEmbed                        (embedFile)
import qualified Data.Map                              as Map
import           Data.Maybe                            (isJust)
import qualified Data.Set                              as Set
import           Data.Streaming.Network                (bindPortTCP,
                                                        bindRandomPortTCP)
import           Data.String                           (fromString)
import           Data.Time                             (getCurrentTime)
import qualified Distribution.Package                  as D
import qualified Distribution.PackageDescription       as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.Simple.Utils             as D
import qualified Distribution.Verbosity                as D
import           Network.HTTP.Client                   (newManager)
import           Network.HTTP.Client                   (managerSetProxy,
                                                        noProxy)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Network.HTTP.ReverseProxy             (ProxyDest (ProxyDest),
                                                        waiProxyToSettings,
                                                        wpsOnExc, wpsTimeout)
import qualified Network.HTTP.ReverseProxy             as ReverseProxy
import           Network.HTTP.Types                    (status200, status503)
import qualified Network.Socket
import           Network.Wai                           (requestHeaderHost,
                                                        requestHeaders,
                                                        responseLBS)
import           Network.Wai.Handler.Warp              (defaultSettings, runSettings,
                                                        setPort, setHost)
import           Network.Wai.Handler.WarpTLS           (runTLS,
                                                        tlsSettingsMemory)
import           Network.Wai.Parse                     (parseHttpAccept)
import           Say
import           System.Directory
import           System.Environment                    (getEnvironment,
                                                        getExecutablePath)
import           System.FilePath                       (takeDirectory,
                                                        takeFileName, (</>))
import           System.FSNotify
import           System.IO                             (stdout, stderr)
import           System.IO.Error                       (isDoesNotExistError)
import           System.Process.Typed

-- We have two special files:
--
-- * The terminate file tells the child process to die simply by being
-- present. Ideally we'd handle this via killing the process
-- directly, but that's historically never worked reliably.
--
-- * The signal file, which tells us that "stack build" has succeeded
-- yet again.
data SpecialFile = TermFile | SignalFile

specialFilePath :: SpecialFile -> FilePath

-- used by scaffolded app, cannot change
specialFilePath TermFile = "yesod-devel/devel-terminate"

-- only used internally, can change
specialFilePath SignalFile = "yesod-devel/rebuild"

-- | Write a special file
writeSpecialFile :: SpecialFile -> IO ()
writeSpecialFile sp = do
    let fp = specialFilePath sp
    createDirectoryIfMissing True $ takeDirectory fp
    now <- getCurrentTime
    writeFile fp $ show now

-- | Remove a special file
removeSpecialFile :: SpecialFile -> IO ()
removeSpecialFile sp = removeFile (specialFilePath sp) `Ex.catch` \e ->
  if isDoesNotExistError e
    then return ()
    else Ex.throwIO e

-- | Get an absolute path to the special file
canonicalizeSpecialFile :: SpecialFile -> IO FilePath
canonicalizeSpecialFile sp = do
    let fp = specialFilePath sp
        dir = takeDirectory fp
        file = takeFileName fp
    createDirectoryIfMissing True dir
    dir' <- canonicalizePath dir
    return $ dir' </> file

-- | Used as a callback from "stack build --exec" to write the signal file
develSignal :: IO ()
develSignal = writeSpecialFile SignalFile

-- | Options to be provided on the command line
data DevelOpts = DevelOpts
      { verbose         :: Bool
      , successHook     :: Maybe String
      , develPort       :: Int
      , develTlsPort    :: Int
      , proxyTimeout    :: Int
      , useReverseProxy :: Bool
      , develHost       :: Maybe String
      } deriving (Show, Eq)

-- | Run a reverse proxy from the develPort and develTlsPort ports to
-- the app running in appPortVar. If there is no response on the
-- application port, give an appropriate message to the user.
reverseProxy :: DevelOpts -> TVar Int -> IO ()
reverseProxy opts appPortVar = do
    manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
    let refreshHtml = LB.fromChunks [$(embedFile "refreshing.html")]
    let onExc _ req
            | maybe False (("application/json" `elem`) . parseHttpAccept)
                (lookup "accept" $ requestHeaders req) =
                    return $ responseLBS status503
                        [ ("Retry-After", "1")
                        ]
                        "{\"message\":\"Recompiling\"}"
            | otherwise = return $ responseLBS status200
                [ ("content-type", "text/html")
                , ("Refresh", "1")
                ]
                refreshHtml

    let proxyApp = waiProxyToSettings
                (const $ do
                    appPort <- atomically $ readTVar appPortVar
                    return $
                        ReverseProxy.WPRProxyDest
                        $ ProxyDest "127.0.0.1" appPort)
                def
                    { wpsOnExc = \e req f -> onExc e req >>= f
                    , wpsTimeout =
                        if proxyTimeout opts == 0
                            then Nothing
                            else Just (1000000 * proxyTimeout opts)
                    }
                manager
        defaultSettings' = maybe id (setHost . fromString) (develHost opts) defaultSettings
        runProxyTls port app = do
          let cert = $(embedFile "certificate.pem")
              key = $(embedFile "key.pem")
              tlsSettings = tlsSettingsMemory cert key
          runTLS tlsSettings (setPort port defaultSettings') $ \req send -> do
            let req' = req
                    { requestHeaders
                        = ("X-Forwarded-Proto", "https")
                        -- Workaround for
                        -- https://github.com/yesodweb/wai/issues/478, where
                        -- the Host headers aren't set. Without this, generated
                        -- URLs from guestApproot are incorrect, see:
                        -- https://github.com/yesodweb/yesod-scaffold/issues/114
                        : (case lookup "host" (requestHeaders req) of
                            Nothing ->
                                case requestHeaderHost req of
                                    Just host -> (("Host", host):)
                                    Nothing -> id
                            Just _ -> id)
                          (requestHeaders req)
                    }
            app req' send
        httpProxy = runSettings (setPort (develPort opts) defaultSettings') proxyApp
        httpsProxy = runProxyTls (develTlsPort opts) proxyApp
    say "Application can be accessed at:\n"
    sayString $ "http://localhost:" ++ show (develPort opts)
    sayString $ "https://localhost:" ++ show (develTlsPort opts)
    say $ "If you wish to test https capabilities, you should set the following variable:"
    sayString $ "  export APPROOT=https://localhost:" ++ show (develTlsPort opts)
    say ""
    race_ httpProxy httpsProxy

-- | Check if the given port is available.
checkPort :: Int -> IO Bool
checkPort p = do
    es <- Ex.tryIO $ bindPortTCP p "*4"
    case es of
        Left _ -> return False
        Right s -> do
            Network.Socket.close s
            return True

-- | Get a random, unused port.
getNewPort :: DevelOpts -> IO Int
getNewPort opts = do
    (port, socket) <- bindRandomPortTCP "*"
    when (verbose opts) $ sayString $ "Got new port: " ++ show port
    Network.Socket.close socket
    return port

-- | Utility function
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c a = c >>= \res -> unless res a

-- | Find the file containing the devel code to be run.
checkDevelFile :: IO FilePath
checkDevelFile =
    loop paths
  where
    paths = ["app/devel.hs", "devel.hs", "src/devel.hs"]

    loop [] = error $ "file devel.hs not found, checked: " ++ show paths
    loop (x:xs) = do
        e <- doesFileExist x
        if e
            then return x
            else loop xs

-- | Get the set of all flags available in the given cabal file
getAvailableFlags :: D.GenericPackageDescription -> Set.Set String
getAvailableFlags =
    Set.fromList . map (unFlagName . D.flagName) . D.genPackageFlags
  where
    unFlagName (D.FlagName fn) = fn

-- | This is the main entry point. Run the devel server.
devel :: DevelOpts -- ^ command line options
      -> [String] -- ^ extra options to pass to Stack
      -> IO ()
devel opts passThroughArgs = do
    -- Check that the listening ports are available
    unlessM (checkPort $ develPort opts) $ error "devel port unavailable"
    unlessM (checkPort $ develTlsPort opts) $ error "devel TLS port unavailable"

    -- Friendly message to the user
    say "Yesod devel server. Enter 'quit' or hit Ctrl-C to quit."

    -- Find out the name of our package, needed for the upcoming Stack
    -- commands
#if MIN_VERSION_Cabal(1, 20, 0)
    cabal  <- D.tryFindPackageDesc "."
#else
    cabal  <- D.findPackageDesc "."
#endif
    gpd    <- D.readPackageDescription D.normal cabal
    let pd = D.packageDescription gpd
        D.PackageIdentifier (D.PackageName packageName) _version = D.package pd

    -- Which file contains the code to run
    develHsPath <- checkDevelFile

    -- The port that we're currently listening on, and that the
    -- reverse proxy should point to
    appPortVar <- newTVarIO (-1)

    -- If we're actually using reverse proxying, spawn off a reverse
    -- proxy thread
    let withRevProxy =
          if useReverseProxy opts
            then race_ (reverseProxy opts appPortVar)
            else id

    -- Run the following concurrently. If any of them exit, take the
    -- whole thing down.
    --
    -- We need to put withChangedVar outside of all this, since we
    -- need to ensure we start watching files before the stack build
    -- loop starts.
    withChangedVar $ \changedVar -> withRevProxy $ race_
        -- Start the build loop
        (runStackBuild appPortVar packageName (getAvailableFlags gpd))

        -- Run the app itself, restarting when a build succeeds
        (runApp appPortVar changedVar develHsPath)
  where
    -- say, but only when verbose is on
    sayV = when (verbose opts) . sayString

    -- Leverage "stack build --file-watch" to do the build
    runStackBuild appPortVar packageName availableFlags = do
        -- We call into this app for the devel-signal command
        myPath <- getExecutablePath
        let procConfig = setStdout createSource
                       $ setStderr createSource
                       $ setDelegateCtlc True $ proc "stack" $
                [ "build"
                , "--fast"
                , "--file-watch"

                -- Indicate the component we want
                , packageName ++ ":lib"

                -- signal the watcher that a build has succeeded
                , "--exec", myPath ++ " devel-signal"
                ] ++

                -- Turn on relevant flags
                concatMap
                    (\flagName -> [ "--flag", packageName ++ ":" ++ flagName])
                    (Set.toList $ Set.intersection
                        availableFlags
                        (Set.fromList ["dev", "library-only"])) ++

                -- Add the success hook
                (case successHook opts of
                    Nothing -> []
                    Just h -> ["--exec", h]) ++

                -- Any extra args passed on the command line
                passThroughArgs

        sayV $ show procConfig

        -- Monitor the stdout and stderr content from the build process. Any
        -- time some output comes, we invalidate the currently running app by
        -- changing the destination port for reverse proxying to -1. We also
        -- make sure that all content to stdout or stderr from the build
        -- process is piped to the actual stdout and stderr handles.
        withProcess_ procConfig $ \p -> do
            let helper getter h =
                      getter p
                   $$ CL.iterM (\_ -> atomically $ writeTVar appPortVar (-1))
                   =$ CB.sinkHandle h
            race_ (helper getStdout stdout) (helper getStderr stderr)

    -- Run the inner action with a TVar which will be set to True
    -- whenever the signal file is modified.
    withChangedVar inner = withManager $ \manager -> do
        -- Variable indicating that the signal file has been changed. We
        -- reset it each time we handle the signal.
        changedVar <- newTVarIO False

        -- Get the absolute path of the signal file, needed for the
        -- file watching
        develSignalFile' <- canonicalizeSpecialFile SignalFile

        -- Start watching the signal file, and set changedVar to
        -- True each time it's changed.
        void $ watchDir manager
                -- Using fromString to work with older versions of fsnotify
                -- that use system-filepath
                (fromString (takeDirectory develSignalFile'))
                (\e -> eventPath e == fromString develSignalFile')
                (const $ atomically $ writeTVar changedVar True)

        -- Run the inner action
        inner changedVar

    -- Each time the library builds successfully, run the application
    runApp appPortVar changedVar develHsPath = do
        -- Wait for the first change, indicating that the library
        -- has been built
        atomically $ do
            changed <- readTVar changedVar
            check changed
            writeTVar changedVar False

        sayV "First successful build complete, running app"

        -- We're going to set the PORT and DISPLAY_PORT variables for
        -- the child below. Also need to know if the env program
        -- exists.
        env <- fmap Map.fromList getEnvironment
        hasEnv <- fmap isJust $ findExecutable "env"

        -- Keep looping forever, print any synchronous exceptions,
        -- and eventually die from an async exception from one of
        -- the other threads (via race_ above).
        forever $ Ex.handleAny (\e -> sayErrString $ "Exception in runApp: " ++ show e) $ do
            -- Get the port the child should listen on, and tell
            -- the reverse proxy about it
            newPort <-
                if useReverseProxy opts
                    then getNewPort opts
                    -- no reverse proxy, so use the develPort directly
                    else return (develPort opts)
            atomically $ writeTVar appPortVar newPort

            -- Modified environment
            let env' = Map.toList
                        $ Map.insert "PORT" (show newPort)
                        $ Map.insert "DISPLAY_PORT" (show $ develPort opts)
                        env

            -- Remove the terminate file so we don't immediately exit
            removeSpecialFile TermFile

            -- Launch the main function in the Main module defined
            -- in the file develHsPath. We use ghc instead of
            -- runghc to avoid the extra (confusing) resident
            -- runghc process. Starting with GHC 8.0.2, that will
            -- not be necessary.

            {- Hmm, unknown errors trying to get this to work. Just doing the
             - runghc thing instead.

            let procDef = setStdin closed $ setEnv env' $ proc "stack"
                    [ "ghc"
                    , "--"
                    , develHsPath
                    , "-e"
                    , "Main.main"
                    ]
            -}

            -- Nix support in Stack doesn't pass along env vars by
            -- default, so we use the env command. But if the command
            -- isn't available, just set the env var. I'm sure this
            -- will break _some_ combination of systems, but we'll
            -- deal with that later. Previous issues:
            --
            -- https://github.com/yesodweb/yesod/issues/1357
            -- https://github.com/yesodweb/yesod/issues/1359
            let procDef
                  | hasEnv = setStdin closed $ proc "stack"
                    [ "exec"
                    , "--"
                    , "env"
                    , "PORT=" ++ show newPort
                    , "DISPLAY_PORT=" ++ show (develPort opts)
                    , "runghc"
                    , develHsPath
                    ]
                  | otherwise = setStdin closed $ setEnv env' $ proc "stack"
                    [ "runghc"
                    , "--"
                    , develHsPath
                    ]

            sayV $ "Running child process: " ++ show procDef

            -- Start running the child process with GHC
            withProcess procDef $ \p -> do
                -- Wait for either the process to exit, or for a new build to come through
                eres <- atomically (fmap Left (waitExitCodeSTM p) <|> fmap Right
                    (do changed <- readTVar changedVar
                        check changed
                        writeTVar changedVar False))
                            -- on an async exception, make sure the child dies
                            `Ex.onException` writeSpecialFile TermFile
                case eres of
                    -- Child exited, which indicates some
                    -- error. Let the user know, sleep for a bit
                    -- to avoid busy-looping, and then we'll try
                    -- again.
                    Left ec -> do
                        sayErrString $ "Unexpected: child process exited with " ++ show ec
                        threadDelay 1000000
                        sayErrString "Trying again"
                    -- New build succeeded
                    Right () -> do
                        -- Kill the child process, both with the
                        -- TermFile, and by signaling the process
                        -- directly.
                        writeSpecialFile TermFile
                        stopProcess p

                        -- Wait until the child properly exits, then we'll try again
                        ec <- waitExitCode p
                        sayV $ "Expected: child process exited with " ++ show ec
