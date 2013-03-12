{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Core.Dispatch
    ( -- * Quasi-quoted routing
      parseRoutes
    , parseRoutesNoCheck
    , parseRoutesFile
    , parseRoutesFileNoCheck
    , mkYesod
    , mkYesodSub
      -- ** More fine-grained
    , mkYesodData
    , mkYesodSubData
    , mkYesodDispatch
    , mkYesodSubDispatch
    , mkDispatchInstance
      -- ** Path pieces
    , PathPiece (..)
    , PathMultiPiece (..)
    , Texts
      -- * Convert to WAI
    , toWaiApp
    , toWaiAppPlain
      -- * WAI subsites
    , WaiSubsite (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Prelude hiding (exp)
import Yesod.Core.Handler

import Web.PathPieces
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Autohead

import Data.ByteString.Lazy.Char8 ()

import Data.Text (Text)
import Data.Monoid (mappend)
import qualified Data.ByteString as S
import qualified Blaze.ByteString.Builder
import Network.HTTP.Types (status301)
import Yesod.Routes.TH
import Yesod.Routes.Parse
import System.Log.FastLogger (Logger)
import Yesod.Core.Types
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Internal.Run

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, /not/ subsites. See 'mkYesodSub' for the latter.
-- Use 'parseRoutes' to create the 'Resource's.
mkYesod :: String -- ^ name of the argument datatype
        -> [ResourceTree String]
        -> Q [Dec]
mkYesod name = fmap (uncurry (++)) . mkYesodGeneral name [] [] False

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating subsites, /not/ sites. See 'mkYesod' for the latter.
-- Use 'parseRoutes' to create the 'Resource's. In general, a subsite is not
-- executable by itself, but instead provides functionality to
-- be embedded in other sites.
mkYesodSub :: String -- ^ name of the argument datatype
           -> Cxt
           -> [ResourceTree String]
           -> Q [Dec]
mkYesodSub name clazzes =
    fmap (uncurry (++)) . mkYesodGeneral name' rest clazzes True
  where
    (name':rest) = words name

-- | Sometimes, you will want to declare your routes in one file and define
-- your handlers elsewhere. For example, this is the only way to break up a
-- monolithic file into smaller parts. Use this function, paired with
-- 'mkYesodDispatch', to do just that.
mkYesodData :: String -> [ResourceTree String] -> Q [Dec]
mkYesodData name res = mkYesodDataGeneral name [] False res

mkYesodSubData :: String -> Cxt -> [ResourceTree String] -> Q [Dec]
mkYesodSubData name clazzes res = mkYesodDataGeneral name clazzes True res

mkYesodDataGeneral :: String -> Cxt -> Bool -> [ResourceTree String] -> Q [Dec]
mkYesodDataGeneral name clazzes isSub res = do
    let (name':rest) = words name
    (x, _) <- mkYesodGeneral name' rest clazzes isSub res
    let rname = mkName $ "resources" ++ name
    eres <- lift res
    let y = [ SigD rname $ ListT `AppT` (ConT ''ResourceTree `AppT` ConT ''String)
            , FunD rname [Clause [] (NormalB eres) []]
            ]
    return $ x ++ y

-- | See 'mkYesodData'.
mkYesodDispatch :: String -> [ResourceTree String] -> Q [Dec]
mkYesodDispatch name = fmap snd . mkYesodGeneral name [] [] False

mkYesodSubDispatch :: String -> Cxt -> [ResourceTree String] -> Q [Dec]
mkYesodSubDispatch name clazzes = fmap snd . mkYesodGeneral name' rest clazzes True 
  where (name':rest) = words name

mkYesodGeneral :: String                   -- ^ foundation type
               -> [String]                 -- ^ arguments for the type
               -> Cxt                      -- ^ the type constraints
               -> Bool                     -- ^ it this a subsite
               -> [ResourceTree String]
               -> Q([Dec],[Dec])
mkYesodGeneral name args clazzes isSub resS = do
     subsite        <- sub
     masterTypeSyns <- if isSub then return   [] 
                                else sequence [handler, widget]
     renderRouteDec <- mkRenderRouteInstance subsite res
     dispatchDec    <- mkDispatchInstance context sub master res
     return (renderRouteDec ++ masterTypeSyns, dispatchDec)
  where sub     = foldl appT subCons subArgs
        master  = if isSub then (varT $ mkName "master") else sub
        context = if isSub then cxt $ yesod : map return clazzes
                           else return []
        yesod   = classP ''Yesod [master]
        handler = tySynD (mkName "Handler") [] [t| GHandler $master $master    |]
        widget  = tySynD (mkName "Widget")  [] [t| GWidget  $master $master () |]
        res     = map (fmap parseType) resS
        subCons = conT $ mkName name
        subArgs = map (varT. mkName) args

-- | If the generation of @'YesodDispatch'@ instance require finer
-- control of the types, contexts etc. using this combinator. You will
-- hardly need this generality. However, in certain situations, like
-- when writing library/plugin for yesod, this combinator becomes
-- handy.
mkDispatchInstance :: CxtQ                -- ^ The context
                   -> TypeQ               -- ^ The subsite type
                   -> TypeQ               -- ^ The master site type
                   -> [ResourceTree a]    -- ^ The resource
                   -> DecsQ
mkDispatchInstance context sub master res = do
  let yDispatch = conT ''YesodDispatch `appT` sub `appT` master
      thisDispatch = do
            clause' <- mkDispatchClause MkDispatchSettings
                { mdsRunHandler = [|yesodRunner|]
                , mdsDispatcher = [|yesodDispatch |]
                , mdsFixEnv = [|fixEnv|]
                , mdsGetPathInfo = [|W.pathInfo|]
                , mdsSetPathInfo = [|\p r -> r { W.pathInfo = p }|]
                , mdsMethod = [|W.requestMethod|]
                } res
            return $ FunD 'yesodDispatch [clause']
   in sequence [instanceD context yDispatch [thisDispatch]]

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This is the same as 'toWaiAppPlain', except it includes two
-- middlewares: GZIP compression and autohead. This is the
-- recommended approach for most users.
toWaiApp :: ( Yesod master
            , YesodDispatch master master
            ) => master -> IO W.Application
toWaiApp y = gzip (gzipSettings y) . autohead <$> toWaiAppPlain y

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This differs from 'toWaiApp' in that it uses no middlewares.
toWaiAppPlain :: ( Yesod master
                 , YesodDispatch master master
                 ) => master -> IO W.Application
toWaiAppPlain a = toWaiApp' a <$> getLogger a <*> makeSessionBackend a


toWaiApp' :: ( Yesod master
             , YesodDispatch master master
             )
          => master
          -> Logger
          -> Maybe (SessionBackend master)
          -> W.Application
toWaiApp' y logger sb req =
    case cleanPath y $ W.pathInfo req of
        Left pieces -> sendRedirect y pieces req
        Right pieces -> yesodDispatch app404 handler405 (yre . Just) req
            { W.pathInfo = pieces
            }
  where
    yre route = YesodRunnerEnv
        { yreLogger = logger
        , yreMaster = y
        , yreSub = y
        , yreToMaster = id
        , yreSessionBackend = sb
        , yreRoute = route
        }
    app404 = yesodRunner (notFound >> return ()) $ yre Nothing
    handler405 = yesodRunner (badMethod >> return ()) . yre . Just

sendRedirect :: Yesod master => master -> [Text] -> W.Application
sendRedirect y segments' env =
     return $ W.responseLBS status301
            [ ("Content-Type", "text/plain")
            , ("Location", Blaze.ByteString.Builder.toByteString dest')
            ] "Redirecting"
  where
    dest = joinPath y (resolveApproot y env) segments' []
    dest' =
        if S.null (W.rawQueryString env)
            then dest
            else (dest `mappend`
                 Blaze.ByteString.Builder.fromByteString (W.rawQueryString env))
