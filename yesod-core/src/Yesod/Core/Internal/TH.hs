{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Core.Internal.TH
    ( mkYesod
    , mkYesodOpts

    , mkYesodWith

    , mkYesodData
    , mkYesodDataOpts

    , mkYesodSubData
    , mkYesodSubDataOpts

    , mkYesodWithParser
    , mkYesodWithParserOpts

    , mkYesodDispatch
    , mkYesodDispatchOpts

    , masterTypeSyns

    , mkYesodGeneral
    , mkYesodGeneralOpts

    , mkMDS
    , mkDispatchInstance

    , mkYesodSubDispatch

    , subTopDispatch

    , RouteOpts
    , defaultOpts
    , setEqDerived
    , setShowDerived
    , setReadDerived
    , setCreateResources
    , setFocusOnNestedRoute
    , setCreateResources
    , setParameterizedSubroute
    , setNestedRouteFallthrough
    )
 where

import Prelude hiding (exp)
import Yesod.Core.Handler
import Language.Haskell.TH hiding (cxt, instanceD)
import Language.Haskell.TH.Syntax
import Data.ByteString.Lazy.Char8 ()
import Data.List (foldl')
import Data.Maybe
import Control.Monad
import Text.Parsec (parse, many1, many, eof, try, option, sepBy1)
import Text.ParserCombinators.Parsec.Char (alphaNum, spaces, string, char)
import Yesod.Routes.TH
import Yesod.Routes.Parse
import Yesod.Core.Types

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, /not/ subsites. See 'mkYesodSubData' and 'mkYesodSubDispatch' for the latter.
-- Use 'parseRoutes' to create the 'Resource's.
--
-- Contexts and type variables in the name of the datatype are parsed.
-- For example, a datatype @App a@ with typeclass constraint @MyClass a@ can be written as @\"(MyClass a) => App a\"@.
mkYesod :: String -- ^ name of the argument datatype
        -> [ResourceTree String]
        -> Q [Dec]
mkYesod = mkYesodOpts defaultOpts

-- | `mkYesod` but with custom options.
--
-- @since 1.6.25.0
mkYesodOpts :: RouteOpts
            -> String
            -> [ResourceTree String]
            -> Q [Dec]
mkYesodOpts opts name = fmap (uncurry (++)) . mkYesodWithParserOpts opts name False return


{-# DEPRECATED mkYesodWith "Contexts and type variables are now parsed from the name in `mkYesod`. <https://github.com/yesodweb/yesod/pull/1366>" #-}
-- | Similar to 'mkYesod', except contexts and type variables are not parsed.
-- Instead, they are explicitly provided.
-- You can write @(MyClass a) => App a@ with @mkYesodWith [[\"MyClass\",\"a\"]] \"App\" [\"a\"] ...@.
mkYesodWith :: [[String]] -- ^ list of contexts
            -> String -- ^ name of the argument datatype
            -> [String] -- ^ list of type variables
            -> [ResourceTree String]
            -> Q [Dec]
mkYesodWith cxts name args = fmap (uncurry (++)) . mkYesodGeneral cxts name args False return


-- | Sometimes, you will want to declare your routes in one file and define
-- your handlers elsewhere. For example, this is the only way to break up a
-- monolithic file into smaller parts. Use this function, paired with
-- 'mkYesodDispatch', to do just that.
mkYesodData :: String -> [ResourceTree String] -> Q [Dec]
mkYesodData = mkYesodDataOpts defaultOpts

-- | `mkYesodData` but with custom options.
--
-- @since 1.6.25.0
mkYesodDataOpts :: RouteOpts -> String -> [ResourceTree String] -> Q [Dec]
mkYesodDataOpts opts name resS = fst <$> mkYesodWithParserOpts opts name False return resS


mkYesodSubData :: String -> [ResourceTree String] -> Q [Dec]
mkYesodSubData = mkYesodSubDataOpts defaultOpts

-- |
--
-- @since 1.6.25.0
mkYesodSubDataOpts :: RouteOpts -> String -> [ResourceTree String] -> Q [Dec]
mkYesodSubDataOpts opts name resS = fst <$> mkYesodWithParserOpts opts name True return resS


-- | Parses contexts and type arguments out of name before generating TH.
mkYesodWithParser :: String                    -- ^ foundation type
                  -> Bool                      -- ^ is this a subsite
                  -> (Exp -> Q Exp)            -- ^ unwrap handler
                  -> [ResourceTree String]
                  -> Q([Dec],[Dec])
mkYesodWithParser = mkYesodWithParserOpts defaultOpts

-- | Parses contexts and type arguments out of name before generating TH.
--
-- @since 1.6.25.0
mkYesodWithParserOpts :: RouteOpts                 -- ^ Additional route options
                      -> String                    -- ^ foundation type
                      -> Bool                      -- ^ is this a subsite
                      -> (Exp -> Q Exp)            -- ^ unwrap handler
                      -> [ResourceTree String]
                      -> Q([Dec],[Dec])
mkYesodWithParserOpts opts name isSub f resS = do
    (name', rest, cxt) <- case parse parseName "" name of
            Left err -> fail $ show err
            Right a -> pure a
    mkYesodGeneralOpts opts cxt name' rest isSub f resS

    where
        parseName = do
            cxt <- option [] parseContext
            name' <- parseWord
            args <- many parseWord
            spaces
            eof
            return ( name', args, cxt)

        parseWord = do
            spaces
            many1 alphaNum

        parseContext = try $ do
            cxts <- parseParen parseContexts
            spaces
            _ <- string "=>"
            return cxts

        parseParen p = do
            spaces
            _ <- char '('
            r <- p
            spaces
            _ <- char ')'
            return r

        parseContexts =
            sepBy1 (many1 parseWord) (spaces >> char ',' >> return ())


-- | See 'mkYesodData'.
mkYesodDispatch :: String -> [ResourceTree String] -> Q [Dec]
mkYesodDispatch = mkYesodDispatchOpts defaultOpts

-- | See 'mkYesodDataOpts'
--
-- @since 1.6.25.0
mkYesodDispatchOpts :: RouteOpts -> String -> [ResourceTree String] -> Q [Dec]
mkYesodDispatchOpts opts name = fmap snd . mkYesodWithParserOpts opts name False return


-- | Get the Handler and Widget type synonyms for the given site.
masterTypeSyns :: [Name] -> Type -> [Dec] -- FIXME remove from here, put into the scaffolding itself?
masterTypeSyns vs site =
    [ TySynD (mkName "Handler") (fmap plainTV vs)
      $ ConT ''HandlerFor `AppT` site
    , TySynD (mkName "Widget")  (fmap plainTV vs)
      $ ConT ''WidgetFor `AppT` site `AppT` ConT ''()
    ]


mkYesodGeneral :: [[String]]                -- ^ Appliction context. Used in RenderRoute, RouteAttrs, and ParseRoute instances.
               -> String                    -- ^ foundation type
               -> [String]                  -- ^ arguments for the type
               -> Bool                      -- ^ is this a subsite
               -> (Exp -> Q Exp)            -- ^ unwrap handler
               -> [ResourceTree String]
               -> Q([Dec],[Dec])
mkYesodGeneral = mkYesodGeneralOpts defaultOpts

-- |
--
-- @since 1.6.25.0
mkYesodGeneralOpts :: RouteOpts                 -- ^ Options to adjust route creation
                   -> [[String]]                -- ^ Application context. Used in RenderRoute, RouteAttrs, and ParseRoute instances.
                   -> String                    -- ^ foundation type
                   -> [String]                  -- ^ arguments for the type
                   -> Bool                      -- ^ is this a subsite
                   -> (Exp -> Q Exp)            -- ^ unwrap handler
                   -> [ResourceTree String]
                   -> Q([Dec],[Dec])
mkYesodGeneralOpts opts appCxt' namestr mtys isSub f resS = do
    let appCxt = fmap (\ctxs ->
            case ctxs of
                c:rest ->
                    foldl' (\acc v -> acc `AppT` fst (nameToType v)) (ConT $ mkName c) rest
                [] -> error $ "Bad context: " ++ show ctxs
          ) appCxt'
    mname <- lookupTypeName namestr
    arity <- case mname of
               Just name -> do
                 info <- reify name
                 return $
                   case info of
                     TyConI dec ->
                       case dec of
                         DataD _ _ vs _ _ _ -> length vs
                         NewtypeD _ _ vs _ _ _ -> length vs
                         TySynD _ vs _ -> length vs
                         _ -> 0
                     _ -> 0
               _ -> return 0
    let name = mkName namestr
    -- Generate as many variable names as the arity indicates
    vns <- replicateM (arity - length mtys) $ newName "t"
    -- types that you apply to get a concrete site name
    let boundNames = fmap nameToType mtys
        argtypes = fmap fst boundNames ++ fmap VarT vns
    -- typevars that should appear in synonym head
    let argvars = (fmap mkName . filter isTvar) mtys ++ vns
        -- Base type (site type with variables)
    let site = foldl' AppT (ConT name) argtypes
        res = map (fmap (parseType . dropBracket)) resS
    renderRouteDec <- mkRenderRouteInstanceOpts opts appCxt boundNames site res
    routeAttrsDec  <-
        case roFocusOnNestedRoute opts of
            Nothing ->
                pure <$> mkRouteAttrsInstance appCxt site res
            Just target ->
                mkRouteAttrsInstanceFor appCxt (ConT (mkName target)) target res

    dispatchDec <- mkDispatchInstance opts site appCxt boundNames f res
    parseRoute <- mkParseRouteInstanceOpts opts boundNames appCxt site res
    let rname = mkName $ "resources" ++ namestr
    resourcesDec <-
        if shouldCreateResources opts
            then do
                eres <- lift resS
                pure
                    [ SigD rname $ ListT `AppT` (ConT ''ResourceTree `AppT` ConT ''String)
                    , FunD rname [Clause [] (NormalB eres) []]
                    ]
            else do
                pure []
    let dataDec = concat
            [ parseRoute
            , renderRouteDec
            , routeAttrsDec
            , if isJust (roFocusOnNestedRoute opts) then [] else resourcesDec
            , if isSub || isJust (roFocusOnNestedRoute opts) then [] else masterTypeSyns argvars site
            ]
    return (dataDec, dispatchDec)
