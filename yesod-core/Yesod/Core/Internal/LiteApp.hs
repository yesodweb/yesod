{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
module Yesod.Core.Internal.LiteApp where

import Yesod.Routes.Class
import Data.Monoid
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Types
import Yesod.Core.Content
import Data.Text (Text)
import Web.PathPieces
import Network.Wai
import Yesod.Core.Handler
import Yesod.Core.Internal.Run
import Network.HTTP.Types (Method)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

newtype LiteApp = LiteApp
    { unLiteApp :: Method -> [Text] -> Maybe (LiteHandler TypedContent)
    }

instance Yesod LiteApp

instance YesodDispatch LiteApp where
    yesodDispatch yre req =
        yesodRunner
            (fromMaybe notFound $ f (requestMethod req) (pathInfo req))
            yre
            (Just $ LiteAppRoute $ pathInfo req)
            req
      where
        LiteApp f = yreSite yre

instance RenderRoute LiteApp where
    data Route LiteApp = LiteAppRoute [Text]
        deriving (Show, Eq, Read, Ord)
    renderRoute (LiteAppRoute x) = (x, [])
instance ParseRoute LiteApp where
    parseRoute (x, _) = Just $ LiteAppRoute x

instance Monoid LiteApp where
    mempty = LiteApp $ \_ _ -> Nothing
    mappend (LiteApp x) (LiteApp y) = LiteApp $ \m ps -> x m ps <|> y m ps

type LiteHandler = HandlerT LiteApp IO
type LiteWidget = WidgetT LiteApp IO

dispatchTo :: ToTypedContent a => LiteHandler a -> LiteApp
dispatchTo handler = LiteApp $ \_ ps ->
    if null ps
        then Just $ fmap toTypedContent handler
        else Nothing

onMethod :: Method -> LiteApp -> LiteApp
onMethod method (LiteApp f) = LiteApp $ \m ps ->
    if method == m
        then f m ps
        else Nothing

onStatic :: Text -> LiteApp -> LiteApp
onStatic p0 (LiteApp f) = LiteApp $ \m ps0 ->
    case ps0 of
        p:ps | p == p0 -> f m ps
        _ -> Nothing

withDynamic :: PathPiece p => (p -> LiteApp) -> LiteApp
withDynamic f = LiteApp $ \m ps0 ->
    case ps0 of
        p:ps | Just v <- fromPathPiece p -> unLiteApp (f v) m ps
        _ -> Nothing

withDynamicMulti :: PathMultiPiece ps => (ps -> LiteApp) -> LiteApp
withDynamicMulti f = LiteApp $ \m ps ->
    case fromPathMultiPiece ps of
        Nothing -> Nothing
        Just v -> unLiteApp (f v) m []
