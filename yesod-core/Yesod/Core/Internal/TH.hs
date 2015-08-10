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
import Data.Either (partitionEithers)

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

mkYesodWith :: String
            -> [Either String [String]]
            -> [ResourceTree String]
            -> Q [Dec]
mkYesodWith name args = fmap (uncurry (++)) . mkYesodGeneral name args False

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
    fmap fst $ mkYesodGeneral name' (fmap Left rest) isSub res

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

-- | 'Left' arguments indicate a monomorphic type, a 'Right' argument
--   indicates a polymorphic type, and provides the list of classes
--   the type must be instance of.
mkYesodGeneral :: String                   -- ^ foundation type
               -> [Either String [String]] -- ^ arguments for the type
               -> Bool                     -- ^ is this a subsite
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
        (mtys,ptys) = partitionEithers args
    -- Generate as many variable names as the arity indicates
    vns <- replicateM (arity - length mtys) $ newName "t"
        -- Base type (site type with variables)
    let (argtypes,cxt) = (\(ns,r,cs) -> (ns ++ fmap VarT r, cs)) $
          foldr (\arg (xs,n:ns,cs) ->
                   case arg of
                     Left  t  -> ( ConT (mkName t):xs, n:ns, cs )
                     Right ts -> ( VarT n         :xs,   ns
                                 , fmap (\t -> AppT (ConT $ mkName t) (VarT n)) ts ++ cs )
                 ) ([],vns,[]) args
        site = foldl' AppT (ConT $ mkName name) argtypes
        res = map (fmap parseType) resS
    renderRouteDec <- mkRenderRouteInstance site res
    routeAttrsDec  <- mkRouteAttrsInstance site res
    dispatchDec    <- mkDispatchInstance site cxt res
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
                   -> Cxt                 -- ^ Context of the instance
                   -> [ResourceTree a]    -- ^ The resource
                   -> DecsQ
mkDispatchInstance master cxt res = do
    clause' <- mkDispatchClause (mkMDS [|yesodRunner|]) res
    let thisDispatch = FunD 'yesodDispatch [clause']
    return [InstanceD cxt yDispatch [thisDispatch]]
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
