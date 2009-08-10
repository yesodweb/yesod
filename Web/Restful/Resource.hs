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

data ResourcePatternPiece =
    Static String
    | Dynamic String
    deriving Show

type ResourcePattern = [ResourcePatternPiece]

fromString :: String -> ResourcePattern
fromString = map fromString' . filter (not . null) . splitOn "/"

fromString' :: String -> ResourcePatternPiece
fromString' ('$':rest) = Dynamic rest
fromString' x = Static x

class Show a => ResourceName a b | a -> b where
    resourcePattern :: a -> String
    allValues :: [a]
    getHandler :: b -> a -> Verb -> Maybe Handler

-- FIXME add some overlap checking functions

type SMap = [(String, String)]

data CheckPatternReturn = StaticMatch | DynamicMatch (String, String) | NoMatch

checkPattern :: ResourcePattern -> Resource -> Maybe SMap
checkPattern rp r =
    if length rp /= length r
        then Nothing
        else combine [] $ zipWith checkPattern' rp r

checkPattern' :: ResourcePatternPiece -> String -> CheckPatternReturn
checkPattern' (Static x) y = if x == y then StaticMatch else NoMatch
checkPattern' (Dynamic x) y = DynamicMatch (x, y)

combine :: SMap -> [CheckPatternReturn] -> Maybe SMap
combine s [] = Just $ reverse s
combine _ (NoMatch:_) = Nothing
combine s (StaticMatch:rest) = combine s rest
combine s (DynamicMatch x:rest) = combine (x:s) rest
