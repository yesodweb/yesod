{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Core.Internal.TH where

import Prelude hiding (exp)
import Yesod.Core.Handler

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W

import Data.ByteString.Lazy.Char8 ()
import Data.List (foldl')
import Control.Monad (replicateM)

import Yesod.Routes.TH
import Yesod.Routes.Parse
import Yesod.Core.Types
import Yesod.Core.Content
import Yesod.Core.Class.Dispatch
import Yesod.Core.Internal.Run

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, /not/ subsites. See 'mkYesodSub' for the latter.
-- Use 'parseRoutes' to create the 'Resource's.
mkYesod :: String -- ^ name of the argument datatype
        -> [ResourceTree String]
        -> Q [Dec]
mkYesod name = fmap (uncurry (++)) . mkYesodGeneral name [] False

-- | Sometimes, you will want to declare your routes in one file and define
-- your handlers elsewhere. For example, this is the only way to break up a
-- monolithic file into smaller parts. Use this function, paired with
-- 'mkYesodDispatch', to do just that.
mkYesodData :: String -> [ResourceTree String] -> Q [Dec]
mkYesodData name res = mkYesodDataGeneral name False res

mkYesodSubData :: String -> [ResourceTree String] -> Q [Dec]
mkYesodSubData name res = mkYesodDataGeneral name True res

mkYesodDataGeneral :: String -> Bool -> [ResourceTree String] -> Q [Dec]
mkYesodDataGeneral name isSub res = do
    let (name':rest) = words name
    fmap fst $ mkYesodGeneral name' rest isSub res

-- | See 'mkYesodData'.
mkYesodDispatch :: String -> [ResourceTree String] -> Q [Dec]
mkYesodDispatch name = fmap snd . mkYesodGeneral name [] False

-- | Get the Handler and Widget type synonyms for the given site.
masterTypeSyns :: [Name] -> Type -> [Dec]
masterTypeSyns vs site =
    [ TySynD (mkName "Handler") (fmap PlainTV vs)
      $ ConT ''HandlerT `AppT` site `AppT` ConT ''IO
    , TySynD (mkName "Widget")  (fmap PlainTV vs)
      $ ConT ''WidgetT `AppT` site `AppT` ConT ''IO `AppT` ConT ''()
    ]

mkYesodGeneral :: String                   -- ^ foundation type
               -> [String]                 -- ^ arguments for the type
               -> Bool                     -- ^ it this a subsite
               -> [ResourceTree String]
               -> Q([Dec],[Dec])
mkYesodGeneral name args isSub resS = do
    info <- reify $ mkName name
    let arity =
          case info of
            TyConI dec ->
              case dec of
                DataD _ _ vs _ _ -> length vs
                NewtypeD _ _ vs _ _ -> length vs
                _ -> 0
            _ -> 0
    -- Generate as many variable names as the arity indicates
    vns <- replicateM arity $ newName "t"
        -- Variables for type parameters
    let vs = fmap VarT vns
        -- Base type (site type with variables)
        basety = foldl' AppT (ConT $ mkName name) vs
        site = foldl' AppT basety (map (VarT . mkName) args)
        res = map (fmap parseType) resS
    renderRouteDec <- mkRenderRouteInstance site res
    routeAttrsDec  <- mkRouteAttrsInstance site res
    dispatchDec    <- mkDispatchInstance site res
    parse <- mkParseRouteInstance site res
    let rname = mkName $ "resources" ++ name
    eres <- lift resS
    let resourcesDec =
            [ SigD rname $ ListT `AppT` (ConT ''ResourceTree `AppT` ConT ''String)
            , FunD rname [Clause [] (NormalB eres) []]
            ]
    let dataDec = concat
            [ [parse]
            , renderRouteDec
            , [routeAttrsDec]
            , resourcesDec
            , if isSub then [] else masterTypeSyns vns site
            ]
    return (dataDec, dispatchDec)

mkMDS :: Q Exp -> MkDispatchSettings
mkMDS rh = MkDispatchSettings
    { mdsRunHandler = rh
    , mdsSubDispatcher =
        [|\parentRunner getSub toParent env -> yesodSubDispatch
                                 YesodSubRunnerEnv
                                    { ysreParentRunner = parentRunner
                                    , ysreGetSub = getSub
                                    , ysreToParentRoute = toParent
                                    , ysreParentEnv = env
                                    }
                              |]
    , mdsGetPathInfo = [|W.pathInfo|]
    , mdsSetPathInfo = [|\p r -> r { W.pathInfo = p }|]
    , mdsMethod = [|W.requestMethod|]
    , mds404 = [|notFound >> return ()|]
    , mds405 = [|badMethod >> return ()|]
    , mdsGetHandler = defaultGetHandler
    }

-- | If the generation of @'YesodDispatch'@ instance require finer
-- control of the types, contexts etc. using this combinator. You will
-- hardly need this generality. However, in certain situations, like
-- when writing library/plugin for yesod, this combinator becomes
-- handy.
mkDispatchInstance :: Type                -- ^ The master site type
                   -> [ResourceTree a]    -- ^ The resource
                   -> DecsQ
mkDispatchInstance master res = do
    clause' <- mkDispatchClause (mkMDS [|yesodRunner|]) res
    let thisDispatch = FunD 'yesodDispatch [clause']
    return [InstanceD [] yDispatch [thisDispatch]]
  where
    yDispatch = ConT ''YesodDispatch `AppT` master

mkYesodSubDispatch :: [ResourceTree a] -> Q Exp
mkYesodSubDispatch res = do
    clause' <- mkDispatchClause (mkMDS [|subHelper . fmap toTypedContent|]) res
    inner <- newName "inner"
    let innerFun = FunD inner [clause']
    helper <- newName "helper"
    let fun = FunD helper
                [ Clause
                    []
                    (NormalB $ VarE inner)
                    [innerFun]
                ]
    return $ LetE [fun] (VarE helper)
