{-# LANGUAGE TemplateHaskell #-}

module Yesod.Routes.TH.ParseRoute
    ( -- ** ParseRoute
      mkParseRouteInstance
    , mkParseRouteInstanceFor
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Data.Text (Text)
import Yesod.Routes.Class
import Yesod.Routes.TH.Dispatch

mkParseRouteInstance :: Cxt -> Type -> [ResourceTree a] -> Q Dec
mkParseRouteInstance cxt typ ress = do
    cls <- mkDispatchClause
        MkDispatchSettings
            { mdsRunHandler = [|\_ _ x _ -> x|]
            , mds404 = [|error "mds404"|]
            , mds405 = [|error "mds405"|]
            , mdsGetPathInfo = [|fst|]
            , mdsMethod = [|error "mdsMethod"|]
            , mdsGetHandler = \_ _ -> [|error "mdsGetHandler"|]
            , mdsSetPathInfo = [|\path (_, queryParams) -> (path, queryParams)|]
            , mdsSubDispatcher = [|\_runHandler _getSub toMaster _env -> fmap toMaster . parseRoute|]
            , mdsUnwrapper = return
            , mdsHandleNestedRoute = Just NestedRouteSettings
                { nrsClassName = ''ParseRouteNested
                , nrsDispatchCall = \restExpr sdc constrExpr _dyns ->
                    [e| fmap $(pure constrExpr) (parseRouteNested ($(pure restExpr), snd $(pure $ reqExp sdc))) |]
                , nrsTargetName = Nothing
                }
            }
        (map removeMethods ress)
    helper <- newName "helper"
    fixer <- [|(\f x -> f () x) :: (() -> ([Text], [(Text, Text)]) -> Maybe (Route a)) -> ([Text], [(Text, Text)]) -> Maybe (Route a)|]
    return $ instanceD cxt (ConT ''ParseRoute `AppT` typ)
        [ FunD 'parseRoute $ return $ Clause
            []
            (NormalB $ fixer `AppE` VarE helper)
            [FunD helper [cls]]
        ]
  where
    -- We do this in order to ski the unnecessary method parsing
    removeMethods (ResourceLeaf res) = ResourceLeaf $ removeMethodsLeaf res
    removeMethods (ResourceParent w x y z) = ResourceParent w x y $ map removeMethods z

    removeMethodsLeaf res = res { resourceDispatch = fixDispatch $ resourceDispatch res }

    fixDispatch (Methods x _) = Methods x []
    fixDispatch x = x

mkParseRouteInstanceFor :: String -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstanceFor target ress = do
    cls <- mkDispatchClause
        MkDispatchSettings
            { mdsRunHandler = [|\_ _ x _ -> x|]
            , mds404 = [|error "mds404"|]
            , mds405 = [|error "mds405"|]
            , mdsGetPathInfo = [|fst|]
            , mdsMethod = [|error "mdsMethod"|]
            , mdsGetHandler = \_ _ -> [|error "mdsGetHandler"|]
            , mdsSetPathInfo = [|\p (_, q) -> (p, q)|]
            , mdsSubDispatcher = [|\_runHandler _getSub toMaster _env -> fmap toMaster . parseRoute|]
            , mdsUnwrapper = return
            , mdsHandleNestedRoute = Just NestedRouteSettings
                { nrsClassName = ''ParseRouteNested
                , nrsDispatchCall = \restExpr sdc constrExpr _dyns ->
                    [e| fmap $(pure constrExpr) (parseRouteNested ($(pure restExpr), snd $(pure $ reqExp sdc))) |]
                , nrsTargetName = Just target
                }
            }
        (focusTarget (map removeMethods ress))
    helper <- newName "helper"
    fixer <- [|(\f x -> f () x) :: (() -> ([Text], [(Text, Text)]) -> Maybe a) -> ([Text], [(Text, Text)]) -> Maybe a|]
    return $ [instanceD [] (ConT ''ParseRouteNested `AppT` ConT (mkName target))
        [ FunD 'parseRouteNested $ return $ Clause
            []
            (NormalB $ fixer `AppE` VarE helper)
            [FunD helper [cls]]
        ]
        ]
  where
    focusTarget =
        foldr k []
      where
        k res acc =
            case res of
                ResourceLeaf _ ->
                    acc
                ResourceParent name _ _ children ->
                    if name == target
                    then res : acc
                    else focusTarget children <> acc

    -- We do this in order to ski the unnecessary method parsing
    removeMethods (ResourceLeaf res) = ResourceLeaf $ removeMethodsLeaf res
    removeMethods (ResourceParent w x y z) = ResourceParent w x y $ map removeMethods z

    removeMethodsLeaf res = res { resourceDispatch = fixDispatch $ resourceDispatch res }

    fixDispatch (Methods x _) = Methods x []
    fixDispatch x = x

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing
