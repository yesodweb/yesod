module Hack.Middleware.CleanPath (cleanPath, splitPath) where

import Hack
import qualified Data.ByteString.Lazy as BS
import Web.Encodings
import Data.List.Split

-- | Performs redirects as per 'splitPath'.
cleanPath :: Middleware
cleanPath app env =
    case splitPath $ pathInfo env of
        Left p -> do
            -- include the query string if there
            let suffix =
                    case queryString env of
                        "" -> ""
                        q@('?':_) -> q
                        q -> '?' : q
            return $! Response 303 [("Location", p ++ suffix)] BS.empty
        Right _ -> app env

-- | Given a certain requested path, return either a corrected path
-- to redirect to or the tokenized path.
--
-- This code corrects for the following issues:
--
-- * It is missing a trailing slash, and there is no period after the
-- last slash.
--
-- * There are any doubled slashes.
splitPath :: String -> Either String [String]
splitPath s =
    let corrected = ats $ rds s
     in if corrected == s
            then Right $ map decodeUrl $ filter (not . null)
                       $ splitOneOf "/" s
            else Left corrected

-- | Remove double slashes
rds :: String -> String
rds [] = []
rds [x] = [x]
rds (a:b:c)
    | a == '/' && b == '/' = rds (b:c)
    | otherwise = a : rds (b:c)

-- | Add a trailing slash if it is missing. Empty string is left alone.
ats :: String -> String
ats [] = []
ats s =
    if last s == '/' || dbs (reverse s)
        then s
        else s ++ "/"

-- | Is there a period before a slash here?
dbs :: String -> Bool
dbs ('/':_) = False
dbs ('.':_) = True
dbs (_:x) = dbs x
dbs [] = False
