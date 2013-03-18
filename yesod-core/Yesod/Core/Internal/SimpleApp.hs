{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
module Yesod.Core.Internal.SimpleApp where

import Yesod.Routes.Dispatch
import Yesod.Routes.Class
import Data.Monoid
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Types
import Yesod.Core.Content
import Data.Text (Text)
import Web.PathPieces
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Network.Wai
import Yesod.Core.Handler
import Yesod.Core.Internal.Run
import Network.HTTP.Types (Method)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

newtype SimpleApp = SimpleApp
    { unSimpleApp :: Method -> [Text] -> Maybe (SimpleHandler TypedContent)
    }

instance Yesod SimpleApp

instance YesodDispatch SimpleApp where
    yesodDispatch yre req =
        yesodRunner
            (fromMaybe notFound $ f (requestMethod req) (pathInfo req))
            yre
            (Just $ SimpleAppRoute $ pathInfo req)
            req
      where
        SimpleApp f = yreSite yre

instance RenderRoute SimpleApp where
    data Route SimpleApp = SimpleAppRoute [Text]
        deriving (Show, Eq, Read, Ord)
    renderRoute (SimpleAppRoute x) = (x, [])
instance ParseRoute SimpleApp where
    parseRoute (x, _) = Just $ SimpleAppRoute x

instance Monoid SimpleApp where
    mempty = SimpleApp $ \_ _ -> Nothing
    mappend (SimpleApp x) (SimpleApp y) = SimpleApp $ \m ps -> x m ps <|> y m ps

type SimpleHandler = HandlerT SimpleApp IO
type SimpleWidget = WidgetT SimpleApp IO

dispatchTo :: ToTypedContent a => SimpleHandler a -> SimpleApp
dispatchTo handler = SimpleApp $ \_ ps ->
    if null ps
        then Just $ fmap toTypedContent handler
        else Nothing

onMethod :: Method -> SimpleApp -> SimpleApp
onMethod method (SimpleApp f) = SimpleApp $ \m ps ->
    if method == m
        then f m ps
        else Nothing

onStatic :: Text -> SimpleApp -> SimpleApp
onStatic p0 (SimpleApp f) = SimpleApp $ \m ps0 ->
    case ps0 of
        p:ps | p == p0 -> f m ps
        _ -> Nothing

withDynamic :: PathPiece p => (p -> SimpleApp) -> SimpleApp
withDynamic f = SimpleApp $ \m ps0 ->
    case ps0 of
        p:ps | Just v <- fromPathPiece p -> unSimpleApp (f v) m ps
        _ -> Nothing

withDynamicMulti :: PathMultiPiece ps => (ps -> SimpleApp) -> SimpleApp
withDynamicMulti f = SimpleApp $ \m ps ->
    case fromPathMultiPiece ps of
        Nothing -> Nothing
        Just v -> unSimpleApp (f v) m []
