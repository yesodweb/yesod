{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | This module simply re-exports from other modules for your convenience.
module Yesod
    ( -- * Re-exports from yesod-core
      module Yesod.Request
    , module Yesod.Content
    , module Yesod.Core
    , module Yesod.Handler
    , module Yesod.Dispatch
    , module Yesod.Widget
    , module Yesod.Form
    , module Yesod.Json
    , module Yesod.Persist
      -- * Running your application
    , warp
    , warpDebug
#if !PRODUCTION
    , develServer
#endif
      -- * Commonly referenced functions/datatypes
    , Application
    , lift
    , liftIO
    , MonadPeelIO
      -- * Utilities
    , showIntegral
    , readIntegral
      -- * Hamlet library
      -- ** Hamlet
    , hamlet
    , xhamlet
    , Hamlet
    , Html
    , renderHamlet
    , renderHtml
    , string
    , preEscapedString
    , cdata
      -- ** Julius
    , julius
    , Julius
    , renderJulius
      -- ** Cassius
    , cassius
    , Cassius
    , renderCassius
    ) where

import Yesod.Content
import Yesod.Dispatch
import Yesod.Core
import Yesod.Handler hiding (runHandler)
import Text.Hamlet
import Text.Cassius
import Text.Julius

import Yesod.Request
import Yesod.Widget
import Yesod.Form
import Yesod.Json
import Yesod.Persist
import Network.Wai (Application)
import Network.Wai.Middleware.Debug
#if !PRODUCTION
import Network.Wai.Handler.DevelServer (runQuit)
#endif
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Peel (MonadPeelIO)

import Network.Wai.Handler.Warp (run)
import System.IO (stderr, hPutStrLn)

import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Attoparsec.Text.Lazy as A
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)

showIntegral :: Integral a => a -> String
showIntegral x = show (fromIntegral x :: Integer)

readIntegral :: Num a => String -> Maybe a
readIntegral s =
    case reads s of
        (i, _):_ -> Just $ fromInteger i
        [] -> Nothing

-- | A convenience method to run an application using the Warp webserver on the
-- specified port. Automatically calls 'toWaiApp'.
warp :: (Yesod a, YesodDispatch a a) => Int -> a -> IO ()
warp port a = toWaiApp a >>= run port

-- | Same as 'warp', but also sends a message to stderr for each request, and
-- an \"application launched\" message as well. Can be useful for development.
warpDebug :: (Yesod a, YesodDispatch a a) => Int -> a -> IO ()
warpDebug port a = do
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    toWaiApp a >>= run port . debug

#if !PRODUCTION
-- | Run a development server, where your code changes are automatically
-- reloaded.
develServer :: Int -- ^ port number
            -> String -- ^ module name holding the code
            -> String -- ^ name of function providing a with-application
            -> IO ()

develServer port modu func = do
    mapM_ putStrLn
        [ "Starting your server process. Code changes will be automatically"
        , "loaded as you save your files. Type \"quit\" to exit."
        , "You can view your app at http://localhost:" ++ show port ++ "/"
        , ""
        ]
    runQuit port modu func determineHamletDeps

#endif

data TempType = Hamlet | Cassius | Julius | Widget
    deriving Show

-- | Determine which Hamlet files a Haskell file depends upon.
determineHamletDeps :: FilePath -> IO [FilePath]
determineHamletDeps x = do
    y <- TIO.readFile x
    let z = A.parse (A.many $ (parser <|> (A.anyChar >> return Nothing))) y
    case z of
        A.Fail{} -> return []
        A.Done _ r -> return $ mapMaybe go r
  where
    go (Just (Hamlet, f)) = Just $ "hamlet/" ++ f ++ ".hamlet"
    go (Just (Widget, f)) = Just $ "hamlet/" ++ f ++ ".hamlet"
    go _ = Nothing
    parser = do
        typ <- (A.string "$(hamletFile " >> return Hamlet)
           <|> (A.string "$(cassiusFile " >> return Cassius)
           <|> (A.string "$(juliusFile " >> return Julius)
           <|> (A.string "$(widgetFile " >> return Widget)
           <|> (A.string "$(Settings.hamletFile " >> return Hamlet)
           <|> (A.string "$(Settings.cassiusFile " >> return Cassius)
           <|> (A.string "$(Settings.juliusFile " >> return Julius)
           <|> (A.string "$(Settings.widgetFile " >> return Widget)
        A.skipWhile isSpace
        _ <- A.char '"'
        y <- A.many1 $ A.satisfy (/= '"')
        _ <- A.char '"'
        A.skipWhile isSpace
        _ <- A.char ')'
        return $ Just (typ, y)
