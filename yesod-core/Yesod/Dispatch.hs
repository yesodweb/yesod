{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Dispatch
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
import Yesod.Internal.Core
import Yesod.Handler hiding (lift)
import Yesod.Widget (GWidget)

import Web.PathPieces
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Autohead

import Data.ByteString.Lazy.Char8 ()

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Monoid (mappend)
import qualified Data.ByteString as S
import qualified Blaze.ByteString.Builder
import Network.HTTP.Types (status301)
import Yesod.Routes.TH
import Yesod.Content (chooseRep)
import Yesod.Routes.Parse
import System.Log.FastLogger (Logger)

type Texts = [Text]

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

mkYesodGeneral :: String -- ^ foundation type
               -> [String]
               -> Cxt -- ^ classes
               -> Bool -- ^ is subsite?
               -> [ResourceTree String]
               -> Q ([Dec], [Dec])
mkYesodGeneral name args clazzes isSub resS = do
    let args' = map mkName args
        arg = foldl AppT (ConT name') $ map VarT args'
    let res = map (fmap parseType) resS
    renderRouteDec <- mkRenderRouteInstance arg res

    let logger = mkName "logger"
    Clause pat body decs <- mkDispatchClause
        [|yesodRunner $(return $ VarE logger)|]
        [|yesodDispatch $(return $ VarE logger)|]
        [|fmap chooseRep|]
        res
    let disp = Clause (VarP logger : pat) body decs
    let master = mkName "master"
    let ctx = if isSub
                then ClassP (mkName "Yesod") [VarT master] : clazzes
                else []
    let ytyp = if isSub
                then ConT ''YesodDispatch `AppT` arg `AppT` VarT master
                else ConT ''YesodDispatch `AppT` arg `AppT` arg
    let yesodDispatch' =
            InstanceD ctx ytyp [FunD (mkName "yesodDispatch") [disp]]

    return (renderRouteDec ++ masterTypSyns, [yesodDispatch'])
  where
    name' = mkName name
    masterTypSyns
        | isSub = []
        | otherwise =
            [ TySynD
                (mkName "Handler")
                []
                (ConT ''GHandler `AppT` ConT name' `AppT` ConT name')
            , TySynD
                (mkName "Widget")
                []
                (ConT ''GWidget `AppT` ConT name' `AppT` ConT name' `AppT` TupleT 0)
            ]

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
toWaiApp' y logger sb env =
    case cleanPath y $ W.pathInfo env of
        Left pieces -> sendRedirect y pieces env
        Right pieces ->
            yesodDispatch logger y y id app404 handler405 method pieces sb env
  where
    app404 = yesodRunner logger notFound y y Nothing id
    handler405 route = yesodRunner logger badMethod y y (Just route) id
    method = decodeUtf8With lenientDecode $ W.requestMethod env

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

-- | Wrap up a normal WAI application as a Yesod subsite.
newtype WaiSubsite = WaiSubsite { runWaiSubsite :: W.Application }

instance RenderRoute WaiSubsite where
    data Route WaiSubsite = WaiSubsiteRoute [Text] [(Text, Text)]
        deriving (Show, Eq, Read, Ord)
    renderRoute (WaiSubsiteRoute ps qs) = (ps, qs)

instance YesodDispatch WaiSubsite master where
    yesodDispatch _logger _master (WaiSubsite app) _tomaster _404 _405 _method _pieces _session = app
