-- | This module should be removed when web-routes incorporates necessary support.
module Yesod.WebRoutes
    ( encodePathInfo
    , Site (..)
    ) where

import Codec.Binary.UTF8.String (encodeString)
import Data.List (intercalate)
import Network.URI

encodePathInfo :: [String] -> [(String, String)] -> String
encodePathInfo pieces qs = 
  let x = map encodeString  `o` -- utf-8 encode the data characters in path components (we have not added any delimiters yet)
          map (escapeURIString (\c -> isUnreserved c || c `elem` ":@&=+$,"))   `o` -- percent encode the characters
          map (\str -> case str of "." -> "%2E" ; ".." -> "%2E%2E" ; _ -> str) `o` -- encode . and ..
          intercalate "/"  -- add in the delimiters
      y = showParams qs
   in x pieces ++ y
    where
      -- reverse composition 
      o :: (a -> b) -> (b -> c) -> a -> c
      o = flip (.)

{-|

A site groups together the three functions necesary to make an application:

* A function to convert from the URL type to path segments.

* A function to convert from path segments to the URL, if possible.

* A function to return the application for a given URL.

There are two type parameters for Site: the first is the URL datatype, the
second is the application datatype. The application datatype will depend upon
your server backend.
-}
data Site url a
    = Site {
           {-|
               Return the appropriate application for a given URL.

               The first argument is a function which will give an appropriate
               URL (as a String) for a URL datatype. This is usually
               constructed by a combination of 'formatPathSegments' and the
               prepending of an absolute application root.

               Well behaving applications should use this function to
               generating all internal URLs.
           -}
             handleSite         :: (url -> [(String, String)] -> String) -> url -> a
           -- | This function must be the inverse of 'parsePathSegments'.
           , formatPathSegments :: url -> ([String], [(String, String)])
           -- | This function must be the inverse of 'formatPathSegments'.
           , parsePathSegments  :: [String] -> Either String url
           }

showParams :: [(String, String)] -> String
showParams [] = ""
showParams z =
    '?' : intercalate "&" (map go z)
  where
    go (x, "") = go' x
    go (x, y) = go' x ++ '=' : go' y
    go' = concatMap encodeUrlChar

-- | Taken straight from web-encodings; reimplemented here to avoid extra
-- dependencies.
encodeUrlChar :: Char -> String
encodeUrlChar c
    -- List of unreserved characters per RFC 3986
    -- Gleaned from http://en.wikipedia.org/wiki/Percent-encoding
    | 'A' <= c && c <= 'Z' = [c]
    | 'a' <= c && c <= 'z' = [c]
    | '0' <= c && c <= '9' = [c]
encodeUrlChar c@'-' = [c]
encodeUrlChar c@'_' = [c]
encodeUrlChar c@'.' = [c]
encodeUrlChar c@'~' = [c]
encodeUrlChar ' ' = "+"
encodeUrlChar y =
    let (a, c) = fromEnum y `divMod` 16
        b = a `mod` 16
        showHex' x
            | x < 10 = toEnum $ x + (fromEnum '0')
            | x < 16 = toEnum $ x - 10 + (fromEnum 'A')
            | otherwise = error $ "Invalid argument to showHex: " ++ show x
     in ['%', showHex' b, showHex' c]
