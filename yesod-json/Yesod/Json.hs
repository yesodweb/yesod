{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Json
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
    ) where

import Yesod.Handler (GHandler, waiRequest, lift, invalidArgs, redirect)
import Yesod.Content
    ( ToContent (toContent), RepHtmlJson (RepHtmlJson), RepHtml (RepHtml)
    , RepJson (RepJson), Content (ContentBuilder)
    )
import Yesod.Core (defaultLayout, Yesod)
import Yesod.Widget (GWidget)
import Yesod.Routes.Class
import Control.Arrow (second)
import Control.Applicative ((<$>))
import Control.Monad (join)
import qualified Data.Aeson as J
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
import Data.Conduit (($$))
import Network.Wai (requestBody, requestHeaders)
import Network.Wai.Parse (parseHttpAccept)
import qualified Data.ByteString.Char8 as B8
import Safe (headMay)

instance ToContent J.Value where
    toContent = flip ContentBuilder Nothing
              . Blaze.fromLazyText
              . toLazyText
              . fromValue

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
    fmap J.fromJSON $ lift $ requestBody req $$ sinkParser J.json'

-- | Same as 'parseJsonBody', but return an invalid args response on a parse
-- error.
parseJsonBody_ :: J.FromJSON a => GHandler sub master a
parseJsonBody_ = do
    ra <- parseJsonBody
    case ra of
        J.Error s -> invalidArgs [pack s]
        J.Success a -> return a

instance ToJavascript J.Value where
    toJavascript = fromLazyText . decodeUtf8 . JE.encode

-- | Convert a list of pairs to an 'J.Object'.
object :: J.ToJSON a => [(Text, a)] -> J.Value
object = J.object . map (second J.toJSON)

-- | Convert a list of values to an 'J.Array'.
array :: J.ToJSON a => [a] -> J.Value
array = J.Array . V.fromList . map J.toJSON

-- | jsonOrRedirect simplifies the scenario where a POST handler sends a different
-- response based on Accept headers:
--
--     1. 200 with JSON data if the client prefers application/json (e.g. AJAX).
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
  where
    acceptsJson = maybe False ((== "application/json") . B8.takeWhile (/= ';'))
                . join
                . fmap (headMay . parseHttpAccept)
                . lookup "Accept" . requestHeaders
                <$> waiRequest
