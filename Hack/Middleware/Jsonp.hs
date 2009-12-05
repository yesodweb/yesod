---------------------------------------------------------
-- |
-- Module        : Hack.Middleware.Jsonp
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic wrapping of JSON responses to convert into JSONP.
--
---------------------------------------------------------
module Hack.Middleware.Jsonp (jsonp) where

import Hack
import Web.Encodings (decodeUrlPairs)
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)

-- | Wrap json responses in a jsonp callback.
--
-- Basically, if the user requested a \"text\/javascript\" and supplied a
-- \"callback\" GET parameter, ask the application for an
-- \"application/json\" response, then convern that into a JSONP response,
-- having a content type of \"text\/javascript\" and calling the specified
-- callback function.
jsonp :: Middleware
jsonp app env = do
    let accept = fromMaybe "" $ lookup "Accept" $ http env
    let gets = decodeUrlPairs $ queryString env
    let callback :: Maybe String
        callback =
            if "text/javascript" `isInfixOf` accept
                then lookup "callback" gets
                else Nothing
    let env' =
            case callback of
                Nothing -> env
                Just _ -> env
                        { http = changeVal "Accept"
                                           "application/json"
                                           $ http env
                        }
    res <- app env'
    let ctype = fromMaybe "" $ lookup "Content-Type" $ headers res
    case callback of
        Nothing -> return res
        Just c ->
          case ctype of
            "application/json" -> return $ res
                { headers = changeVal "Content-Type"
                                      "text/javascript"
                                      $ headers res
                , body = B8.concat
                            [ B8.pack c -- NOTE uses Latin-1 encoding.
                            , B8.singleton '('
                            , body res
                            , B8.singleton ')'
                            ]
                }
            _ -> return res

changeVal :: String -> String -> [(String, String)] -> [(String, String)]
changeVal key val old = (key, val) : filter (\(k, _) -> k /= key) old
