{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Resource
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Defines the Resource class.
--
---------------------------------------------------------
module Web.Restful.Resource
    ( ResourceName (..)
    , fromString
    , checkPattern
    ) where

import Data.List.Split (splitOn)
import Web.Restful.Definitions
import Web.Restful.Handler
import Data.List (intercalate)

data ResourcePatternPiece =
    Static String
    | Dynamic String
    | Slurp String -- ^ take up the rest of the pieces. must be last
    deriving Show

isSlurp :: ResourcePatternPiece -> Bool
isSlurp (Slurp _) = True
isSlurp _ = False

type ResourcePattern = [ResourcePatternPiece]

fromString :: String -> ResourcePattern
fromString = map fromString' . filter (not . null) . splitOn "/"

fromString' :: String -> ResourcePatternPiece
fromString' ('$':rest) = Dynamic rest
fromString' ('*':rest) = Slurp rest
fromString' x = Static x

class Show a => ResourceName a b | a -> b where
    -- | Get the URL pattern for each different resource name.
    -- Something like /foo/$bar/baz/ will match the regular expression
    -- /foo/(\\w*)/baz/, matching the middle part with the urlParam bar.
    --
    -- Also, /foo/*bar/ will match /foo/[anything else], capturing the value
    -- into the bar urlParam.
    resourcePattern :: a -> String

    -- | Get all possible values for resource names.
    -- Remember, if you use variables ($foo) in your resourcePatterns you
    -- can get an unlimited number of resources for each resource name.
    allValues :: [a]

    -- | Find the handler for each resource name/verb pattern.
    getHandler :: b -> a -> Verb -> Handler

-- FIXME add some overlap checking functions

type SMap = [(String, String)]

data CheckPatternReturn = StaticMatch | DynamicMatch (String, String) | NoMatch

checkPattern :: ResourcePattern -> Resource -> Maybe SMap
checkPattern rp r
    | length rp /= 0 && isSlurp (last rp) = do
        let rp' = init rp
            (r1, r2) = splitAt (length rp') r
        smap <- checkPattern rp' r1
        let slurpValue = intercalate "/" r2
            Slurp slurpKey = last rp
        return $ (slurpKey, slurpValue) : smap
    | length rp /= length r = Nothing
    | otherwise = combine [] $ zipWith checkPattern' rp r

checkPattern' :: ResourcePatternPiece -> String -> CheckPatternReturn
checkPattern' (Static x) y = if x == y then StaticMatch else NoMatch
checkPattern' (Dynamic x) y = DynamicMatch (x, y)
checkPattern' (Slurp x) _ = error $ "Slurp pattern " ++ x ++ " must be last"

combine :: SMap -> [CheckPatternReturn] -> Maybe SMap
combine s [] = Just $ reverse s
combine _ (NoMatch:_) = Nothing
combine s (StaticMatch:rest) = combine s rest
combine s (DynamicMatch x:rest) = combine (x:s) rest
