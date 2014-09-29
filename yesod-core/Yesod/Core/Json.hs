{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Core.Json
    ( -- * Convert from a JSON value
      defaultLayoutJson
    , jsonToRepJson
    , returnJson
    , provideJson

      -- * Convert to a JSON value
    , parseJsonBody
    , parseJsonBody_
    , requireJsonBody

      -- * Produce JSON values
    , J.Value (..)
    , J.ToJSON (..)
    , J.FromJSON (..)
    , array
    , object
    , (.=)
    , (J..:)

      -- * Convenience functions
    , jsonOrRedirect
    , acceptsJson
    ) where

import Yesod.Core.Handler (HandlerT, getRequest, invalidArgs, redirect, selectRep, provideRep, rawRequestBody, ProvidedRep)
import Control.Monad.Trans.Writer (Writer)
import Data.Monoid (Endo)
import Yesod.Core.Content (TypedContent)
import Yesod.Core.Types (reqAccept)
import Yesod.Core.Class.Yesod (defaultLayout, Yesod)
import Yesod.Core.Class.Handler
import Yesod.Core.Widget (WidgetT)
import Yesod.Routes.Class
import qualified Data.Aeson as J
import qualified Data.Aeson.Parser as JP
import Data.Aeson ((.=), object)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Text (pack)
import qualified Data.Vector as V
import Data.Conduit
import Data.Conduit.Lift
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (listToMaybe)
import Control.Monad (liftM)

-- | Provide both an HTML and JSON representation for a piece of
-- data, using the default layout for the HTML output
-- ('defaultLayout').
--
-- /Since: 0.3.0/
defaultLayoutJson :: (Yesod site, J.ToJSON a)
                  => WidgetT site IO ()  -- ^ HTML
                  -> HandlerT site IO a  -- ^ JSON
                  -> HandlerT site IO TypedContent
defaultLayoutJson w json = selectRep $ do
    provideRep $ defaultLayout w
    provideRep $ fmap J.toJSON json

-- | Wraps a data type in a 'RepJson'.  The data type must
-- support conversion to JSON via 'J.ToJSON'.
--
-- /Since: 0.3.0/
jsonToRepJson :: (Monad m, J.ToJSON a) => a -> m J.Value
jsonToRepJson = return . J.toJSON
{-# DEPRECATED jsonToRepJson "Use returnJson instead" #-}

-- | Convert a value to a JSON representation via aeson\'s 'J.toJSON' function.
--
-- Since 1.2.1
returnJson :: (Monad m, J.ToJSON a) => a -> m J.Value
returnJson = return . J.toJSON

-- | Provide a JSON representation for usage with 'selectReps', using aeson\'s
-- 'J.toJSON' function to perform the conversion.
--
-- Since 1.2.1
provideJson :: (Monad m, J.ToJSON a) => a -> Writer (Endo [ProvidedRep m]) ()
provideJson = provideRep . return . J.toJSON

-- | Parse the request body to a data type as a JSON value.  The
-- data type must support conversion from JSON via 'J.FromJSON'.
-- If you want the raw JSON value, just ask for a @'J.Result'
-- 'J.Value'@.
--
-- Note that this function will consume the request body. As such, calling it
-- twice will result in a parse error on the second call, since the request
-- body will no longer be available.
--
-- /Since: 0.3.0/
parseJsonBody :: (MonadHandler m, J.FromJSON a) => m (J.Result a)
parseJsonBody = do
    eValue <- rawRequestBody $$ runCatchC (sinkParser JP.value')
    return $ case eValue of
        Left e -> J.Error $ show e
        Right value -> J.fromJSON value

-- | Same as 'parseJsonBody', but return an invalid args response on a parse
-- error.
parseJsonBody_ :: (MonadHandler m, J.FromJSON a) => m a
parseJsonBody_ = requireJsonBody
{-# DEPRECATED parseJsonBody_ "Use requireJsonBody instead" #-}

-- | Same as 'parseJsonBody', but return an invalid args response on a parse
-- error.
requireJsonBody :: (MonadHandler m, J.FromJSON a) => m a
requireJsonBody = do
    ra <- parseJsonBody
    case ra of
        J.Error s -> invalidArgs [pack s]
        J.Success a -> return a

-- | Convert a list of values to an 'J.Array'.
array :: J.ToJSON a => [a] -> J.Value
array = J.Array . V.fromList . map J.toJSON

-- | jsonOrRedirect simplifies the scenario where a POST handler sends a different
-- response based on Accept headers:
--
--     1. 200 with JSON data if the client prefers
--     @application\/json@ (e.g. AJAX, see 'acceptsJSON').
--
--     2. 3xx otherwise, following the PRG pattern.
jsonOrRedirect :: (MonadHandler m, J.ToJSON a)
               => Route (HandlerSite m) -- ^ Redirect target
               -> a            -- ^ Data to send via JSON
               -> m J.Value
jsonOrRedirect r j = do
    q <- acceptsJson
    if q then return (J.toJSON j)
         else redirect r

-- | Returns @True@ if the client prefers @application\/json@ as
-- indicated by the @Accept@ HTTP header.
acceptsJson :: MonadHandler m => m Bool
acceptsJson =  (maybe False ((== "application/json") . B8.takeWhile (/= ';'))
            .  listToMaybe
            .  reqAccept)
           `liftM` getRequest
