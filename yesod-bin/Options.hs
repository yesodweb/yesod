{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}

module Options (injectDefaults) where

import           Control.Applicative
import qualified Control.Exception         as E
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Char                 (isAlphaNum, isSpace, toLower)
import           Data.List                 (foldl')
import           Data.List.Split           (splitOn)
import qualified Data.Map                  as M
import           Data.Maybe                (mapMaybe)
import           Data.Monoid
import           Options.Applicative
import           Options.Applicative.Types
import           System.Directory
import           System.Environment
import           System.FilePath           ((</>))

-- | inject defaults from either files or environments
--   in order of priority:
--    1. command line arguments: --long-option=value
--    2. environment variables: PREFIX_COMMAND_LONGOPTION=value
--    3. $HOME/.prefix/config:  prefix.command.longoption=value
--
-- note: this automatically injects values for standard options and flags
--       (also inside subcommands), but not for more complex parsers that use BindP
--       (like `many'). As a workaround a single special case is supported,
--       for `many' arguments that generate a list of strings.

injectDefaults :: String                                     -- ^ prefix, program name
               -> [(String, a -> [String] -> a)]             -- ^ append extra options for arguments that are lists of strings
               -> ParserInfo a                               -- ^ original parsers
               -> IO (ParserInfo a)
injectDefaults prefix lenses parser = do
  e      <- getEnvironment
  config <- (readFile . (</> "config") =<< getAppUserDataDirectory prefix)
              `E.catch` \(_::E.SomeException) -> return ""
  let env = M.fromList . filter ((==[prefix]) . take 1 . fst) $
               configLines config <>                              -- config first
               map (\(k,v) -> (splitOn "_" $ map toLower k, v)) e -- env vars override config
      p' =  parser { infoParser = injectDefaultP env [prefix] (infoParser parser) }
  return $ foldl' (\p (key,l) -> fmap (updateA env key l) p) p' lenses

updateA :: M.Map [String] String -> String -> (a -> [String] -> a) -> a -> a
updateA env key upd a =
  case M.lookup (splitOn "." key) env of
    Nothing -> a
    Just v  -> upd a (splitOn ":" v)

-- | really simple key/value file reader:   x.y = z -> (["x","y"],"z")
configLines :: String -> [([String], String)]
configLines = mapMaybe (mkLine . takeWhile (/='#')) . lines
  where
    trim = let f = reverse . dropWhile isSpace in f . f
    mkLine l | (k, '=':v) <- break (=='=') l = Just (splitOn "." (trim k), trim v)
             | otherwise                       = Nothing

-- | inject the environment into the parser
--   the map contains the paths with the value that's passed into the reader if the
--   command line parser gives no result
injectDefaultP :: M.Map [String] String -> [String] -> Parser a -> Parser a
injectDefaultP _env _path n@(NilP{})   = n
injectDefaultP env path p@(OptP o)
  | (Option (CmdReader cmds f) props) <- o  =
     let cmdMap = M.fromList (map (\c -> (c, mkCmd c)) cmds)
         mkCmd cmd =
           let (Just parseri) = f cmd
           in  parseri { infoParser = injectDefaultP env (path ++ [normalizeName cmd]) (infoParser parseri) }
     in  OptP (Option (CmdReader cmds (`M.lookup` cmdMap)) props)
  | (Option (OptReader names (CReader _ rdr) _) _) <- o =
     p <|> either (const empty)
                  pure
                  (runExcept . msum $
                    map (maybe (throwE $ ErrorMsg "Missing environment variable")
                               (runReaderT (unReadM rdr))
                          . getEnvValue env path)
                        names)
  | (Option (FlagReader names a) _) <- o =
     p <|> if any ((==Just "1") . getEnvValue env path) names then pure a else empty
  | otherwise = p
injectDefaultP env path (MultP p1 p2) =
   MultP (injectDefaultP env path p1) (injectDefaultP env path p2)
injectDefaultP env path (AltP p1 p2) =
   AltP (injectDefaultP env path p1) (injectDefaultP env path p2)
injectDefaultP _env _path b@(BindP {}) = b

getEnvValue :: M.Map [String] String -> [String] -> OptName -> Maybe String
getEnvValue env path (OptLong l) = M.lookup (path ++ [normalizeName l]) env
getEnvValue _ _ _                = Nothing

normalizeName :: String -> String
normalizeName = map toLower . filter isAlphaNum

