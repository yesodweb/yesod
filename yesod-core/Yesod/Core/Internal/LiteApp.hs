{-# LANGUAGE TypeFamilies, PatternGuards, CPP #-}
module Yesod.Core.Internal.LiteApp where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Yesod.Routes.Class
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
import Control.Monad.Trans.Writer

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

liteApp :: Writer LiteApp () -> LiteApp
liteApp = execWriter

dispatchTo :: ToTypedContent a => LiteHandler a -> Writer LiteApp ()
dispatchTo handler = tell $ LiteApp $ \_ ps ->
    if null ps
        then Just $ fmap toTypedContent handler
        else Nothing

onMethod :: Method -> Writer LiteApp () -> Writer LiteApp ()
onMethod method f = tell $ LiteApp $ \m ps ->
    if method == m
        then unLiteApp (liteApp f) m ps
        else Nothing

onStatic :: Text -> Writer LiteApp () -> Writer LiteApp ()
onStatic p0 f = tell $ LiteApp $ \m ps0 ->
    case ps0 of
        p:ps | p == p0 -> unLiteApp (liteApp f) m ps
        _ -> Nothing

withDynamic :: PathPiece p => (p -> Writer LiteApp ()) -> Writer LiteApp ()
withDynamic f = tell $ LiteApp $ \m ps0 ->
    case ps0 of
        p:ps | Just v <- fromPathPiece p -> unLiteApp (liteApp $ f v) m ps
        _ -> Nothing

withDynamicMulti :: PathMultiPiece ps => (ps -> Writer LiteApp ()) -> Writer LiteApp ()
withDynamicMulti f = tell $ LiteApp $ \m ps ->
    case fromPathMultiPiece ps of
        Nothing -> Nothing
        Just v -> unLiteApp (liteApp $ f v) m []
