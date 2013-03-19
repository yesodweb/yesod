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

import Yesod.Routes.TH
import Yesod.Routes.Parse
import Yesod.Core.Types
import Yesod.Core.Content
import Yesod.Core.Class.Dispatch
import Yesod.Core.Internal.Run
import Yesod.Routes.Class
import Data.Text (Text)
import qualified Data.ByteString.Char8 as S8

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
    (x, _) <- mkYesodGeneral name' rest isSub res
    let rname = mkName $ "resources" ++ name
    eres <- lift res
    let y = [ SigD rname $ ListT `AppT` (ConT ''ResourceTree `AppT` ConT ''String)
            , FunD rname [Clause [] (NormalB eres) []]
            ]
    return $ x ++ y

-- | See 'mkYesodData'.
mkYesodDispatch :: String -> [ResourceTree String] -> Q [Dec]
mkYesodDispatch name = fmap snd . mkYesodGeneral name [] False

-- | Get the Handler and Widget type synonyms for the given site.
masterTypeSyns :: Type -> [Dec]
masterTypeSyns site =
    [ TySynD (mkName "Handler") []
      $ ConT ''HandlerT `AppT` site `AppT` ConT ''IO
    , TySynD (mkName "Widget")  []
      $ ConT ''WidgetT `AppT` site `AppT` ConT ''IO `AppT` ConT ''()
    ]

mkYesodGeneral :: String                   -- ^ foundation type
               -> [String]                 -- ^ arguments for the type
               -> Bool                     -- ^ it this a subsite
               -> [ResourceTree String]
               -> Q([Dec],[Dec])
mkYesodGeneral name args isSub resS = do
     renderRouteDec <- mkRenderRouteInstance site res
     dispatchDec    <- mkDispatchInstance site res
     parse <- mkParseRouteInstance site res
     return (parse : renderRouteDec ++ if isSub then [] else masterTypeSyns site, dispatchDec)
  where site    = foldl' AppT (ConT $ mkName name) (map (VarT . mkName) args)
        res     = map (fmap parseType) resS

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
