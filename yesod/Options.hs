{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Options (injectDefaults) where

import           Control.Applicative
import qualified Control.Exception         as E
import           Control.Monad
import           Data.Char                 (isAlphaNum, isSpace, toLower)
import           Data.List.Split           (splitOn)
import qualified Data.Map                  as M
import           Data.Maybe                (catMaybes)
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
injectDefaults :: String -> ParserInfo a -> IO (ParserInfo a)
injectDefaults prefix parser = do
  e      <- getEnvironment
  config <- (readFile . (</> "config") =<< getAppUserDataDirectory prefix)
              `E.catch` \(_::E.SomeException) -> return ""
  let env = M.fromList . filter ((==[prefix]) . take 1 . fst) $
               configLines config <>                              -- config first
               map (\(k,v) -> (splitOn "_" $ map toLower k, v)) e -- env vars override config
  print env
  return $ parser { infoParser = injectDefaultP env [prefix] (infoParser parser) }

-- | really simple key/value file reader:   x.y = z -> (["x","y"],"z")
configLines :: String -> [([String], String)]
configLines = catMaybes . map (mkLine . takeWhile (/='#')) . lines
  where
    trim = let f = reverse . dropWhile isSpace in f . f
    mkLine l | (k, ('=':v)) <- break (=='=') l = Just (splitOn "." (trim k), trim v)
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
  | (Option (OptReader names (CReader _ rdr)) _) <- o =
     p <|> maybe empty pure (msum $ map (rdr <=< getEnvValue env path) names)
  | (Option (FlagReader names a) _) <- o =
     p <|> if any ((==Just "1") . getEnvValue env path) names then pure a else empty
  | otherwise = p
injectDefaultP env path (MultP p1 p2) =
   MultP (injectDefaultP env path p1) (injectDefaultP env path p2)
injectDefaultP env path (AltP p1 p2) =
   AltP (injectDefaultP env path p1) (injectDefaultP env path p2)
injectDefaultP env path (BindP p1 f) =
   BindP (injectDefaultP env path p1) (\a -> injectDefaultP env path (f a))

getEnvValue :: M.Map [String] String -> [String] -> OptName -> Maybe String
getEnvValue env path (OptLong l) = M.lookup (path ++ [normalizeName l]) env
getEnvValue _ _ _                = Nothing

normalizeName :: String -> String
normalizeName = map toLower . filter isAlphaNum
