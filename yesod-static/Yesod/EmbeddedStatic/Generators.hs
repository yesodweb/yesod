{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}
-- | A generator is executed at compile time to load a list of entries
-- to embed into the subsite.  This module contains several basic generators,
-- but the design of generators and entries is such that it is straightforward
-- to make custom generators for your own specific purposes, see <#g:4 this section>.
module Yesod.EmbeddedStatic.Generators (
  -- * Generators
    Location

  -- * Util
  , pathToName
  
  -- * Custom Generators
  
  -- $example
) where

import Data.Char (isDigit, isLower)
import Language.Haskell.TH

import Yesod.EmbeddedStatic.Types

-- | Clean up a path to make it a valid haskell name by replacing all non-letters
--   and non-numbers by underscores.  In addition, if the path starts with a capital
--   letter or number add an initial underscore.
pathToName :: FilePath -> Name
pathToName f = routeName
    where
      replace c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'
      name = map replace f
      routeName = mkName $
            case () of
                ()
                    | null name -> error "null-named file"
                    | isDigit (head name) -> '_' : name
                    | isLower (head name) -> name
                    | otherwise -> '_' : name


-- $example
-- Here is an example of creating your own custom generator.
-- Because of template haskell stage restrictions, you must define generators in a
-- different module from where you use them.  The following generator will embed a
-- JSON document that contains the compile time.
--
-- >{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
-- >module CompileTime where
-- >
-- >import Data.Aeson
-- >import Data.Time
-- >import Yesod.EmbeddedStatic.Generators
-- >import Yesod.EmbeddedStatic.Types
-- >import qualified Data.ByteString.Lazy as BL
-- >
-- >getTime :: IO BL.ByteString
-- >getTime = do
-- >    t <- getCurrentTime
-- >    return $ encode $
-- >        object [ "compile_time" .= show t ]
-- >
-- >timeGenerator :: Location -> Generator
-- >timeGenerator loc =
-- >    return $ [Entry
-- >        { ebHaskellName = Just $ pathToName loc
-- >        , ebLocation    = loc
-- >        , ebMimeType    = "application/json"
-- >        , ebProductionContent = getTime
-- >        , ebDevelReload = [| getTime |]
-- >        , ebDevelExtraFiles = Nothing
-- >        }]
--
-- Notice how the @getTime@ action is given as both 'ebProductionContent' and
-- 'ebDevelReload'.  The result is that during development, the @getTime@ action
-- will be re-executed on every request so the time returned will be different
-- for each reload.  When compiling for production, the @getTime@ action will
-- be executed once at compile time to produce the content to embed and never
-- called at runtime.
--
-- Here is a small example yesod program using this generator.  Try toggling
-- the development argument to @mkEmbeddedStatic@.
-- 
-- >{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}
-- >module Main where
-- >
-- >import Yesod
-- >import Yesod.EmbeddedStatic
-- >import CompileTime (timeGenerator)
-- >
-- >mkEmbeddedStatic True "eStatic" [timeGenerator "compile-time.json"]
-- >
-- >-- The above will generate variables
-- >-- eStatic :: EmbeddedStatic
-- >-- compile_time_json :: Route EmbeddedStatic
-- >
-- >data MyApp = MyApp { getStatic :: EmbeddedStatic }
-- >
-- >mkYesod "MyApp" [parseRoutes|
-- >/ HomeR GET
-- >/static StaticR EmbeddedStatic getStatic
-- >|]
-- >
-- >instance Yesod MyApp
-- >
-- >getHomeR :: Handler Html
-- >getHomeR = defaultLayout $ [whamlet|
-- ><h1>Hello
-- ><p>Check the 
-- >    <a href=@{StaticR compile_time_json}>compile time
-- >|]
-- >
-- >main :: IO ()
-- >main = warp 3000 $ MyApp eStatic
