{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Core.Json
    ( -- * Convert from a JSON value
      defaultLayoutJson
    , jsonToRepJson

      -- * Convert to a JSON value
    , parseJsonBody
    , parseJsonBody_

      -- * Produce JSON values
    , J.Value (..)
    , object
    , array
    , (.=)

      -- * Convenience functions
    , jsonOrRedirect
    , acceptsJson
    ) where

import Yesod.Handler (GHandler, waiRequest, lift, invalidArgs, redirect)
import Yesod.Content
    ( ToContent (toContent), RepHtmlJson (RepHtmlJson), RepHtml (RepHtml)
    , RepJson (RepJson), Content (ContentBuilder)
    )
import Yesod.Internal.Core (defaultLayout, Yesod)
import Yesod.Widget (GWidget)
import Yesod.Routes.Class
import Control.Arrow (second)
import Control.Applicative ((<$>))
import Control.Monad (join)
import qualified Data.Aeson as J
import qualified Data.Aeson.Parser as JP
import Data.Aeson ((.=))
import qualified Data.Aeson.Encode as JE
import Data.Aeson.Encode (fromValue)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Text (Text, pack)
import qualified Data.Vector as V
import Text.Julius (ToJavascript (..))
import Data.Text.Lazy.Builder (fromLazyText)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import Data.Conduit
import Network.Wai (requestBody, requestHeaders)
import Network.Wai.Parse (parseHttpAccept)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (listToMaybe)

-- | Provide both an HTML and JSON representation for a piece of
-- data, using the default layout for the HTML output
-- ('defaultLayout').
--
-- /Since: 0.3.0/
defaultLayoutJson :: (Yesod master, J.ToJSON a)
                  => GWidget sub master ()  -- ^ HTML
                  -> a                      -- ^ JSON
                  -> GHandler sub master RepHtmlJson
defaultLayoutJson w json = do
    RepHtml html' <- defaultLayout w
    return $ RepHtmlJson html' $ toContent (J.toJSON json)

-- | Wraps a data type in a 'RepJson'.  The data type must
-- support conversion to JSON via 'J.ToJSON'.
--
-- /Since: 0.3.0/
jsonToRepJson :: J.ToJSON a => a -> GHandler sub master RepJson
jsonToRepJson = return . RepJson . toContent . J.toJSON

-- | Parse the request body to a data type as a JSON value.  The
-- data type must support conversion from JSON via 'J.FromJSON'.
-- If you want the raw JSON value, just ask for a @'J.Result'
-- 'J.Value'@.
--
-- /Since: 0.3.0/
parseJsonBody :: J.FromJSON a => GHandler sub master (J.Result a)
parseJsonBody = do
    req <- waiRequest
    eValue <- lift
            $ runExceptionT
            $ transPipe lift (requestBody req)
           $$ sinkParser JP.value'
    return $ case eValue of
        Left e -> J.Error $ show e
        Right value -> J.fromJSON value

-- | Same as 'parseJsonBody', but return an invalid args response on a parse
-- error.
parseJsonBody_ :: J.FromJSON a => GHandler sub master a
parseJsonBody_ = do
    ra <- parseJsonBody
    case ra of
        J.Error s -> invalidArgs [pack s]
        J.Success a -> return a

#if !MIN_VERSION_shakespeare_js(1, 0, 2)
instance ToJavascript J.Value where
    toJavascript = fromLazyText . decodeUtf8 . JE.encode
#endif

-- | Convert a list of pairs to an 'J.Object'.
object :: J.ToJSON a => [(Text, a)] -> J.Value
object = J.object . map (second J.toJSON)

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
jsonOrRedirect :: (Yesod master, J.ToJSON a)
               => Route master -- ^ Redirect target
               -> a            -- ^ Data to send via JSON
               -> GHandler sub master RepJson
jsonOrRedirect r j = do
    q <- acceptsJson
    if q then jsonToRepJson (J.toJSON j)
         else redirect r

-- | Returns @True@ if the client prefers @application\/json@ as
-- indicated by the @Accept@ HTTP header.
acceptsJson :: Yesod master => GHandler sub master Bool
acceptsJson =  maybe False ((== "application/json") . B8.takeWhile (/= ';'))
            .  join
            .  fmap (listToMaybe . parseHttpAccept)
            .  lookup "Accept" . requestHeaders
           <$> waiRequest
