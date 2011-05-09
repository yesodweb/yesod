{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Yesod.Form.Fields
    ( textField
    , passwordField
    , textareaField
    , hiddenField
    , intField
    , dayField
    , timeField
    , htmlField
    , emailField
    , searchField
    , AutoFocus
    , urlField
    , doubleField
    , parseDate
    , parseTime
    , Textarea (..)
    ) where

import Yesod.Form.Types
import Yesod.Widget
import Text.Hamlet hiding (renderHtml)
import Text.Blaze (ToHtml (..))
import Text.Cassius
import Data.Time (Day, TimeOfDay(..))
import qualified Text.Email.Validate as Email
import Network.URI (parseURI)
import Database.Persist (PersistField)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Control.Monad (when)

import qualified Blaze.ByteString.Builder.Html.Utf8 as B
import Blaze.ByteString.Builder (writeByteString, toLazyByteString)
import Blaze.ByteString.Builder.Internal.Write (fromWriteList)

import Text.Blaze.Renderer.String (renderHtml)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text, unpack, pack)

#if __GLASGOW_HASKELL__ >= 700
#define WHAMLET whamlet
#define HAMLET hamlet
#define CASSIUS cassius
#define JULIUS julius
#else
#define WHAMLET $whamlet
#define HAMLET $hamlet
#define CASSIUS $cassius
#define JULIUS $julius
#endif

intField :: (Monad monad, Integral i) => Field (GGWidget master monad ()) i
intField = Field
    { fieldParse = maybe (Left "Invalid integer") Right . readMayI . unpack -- FIXME Data.Text.Read
    , fieldRender = pack . showI
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="number" :isReq:required="" value="#{val}">
|]
    }
  where
    showI x = show (fromIntegral x :: Integer)
    readMayI s = case reads s of
                    (x, _):_ -> Just $ fromInteger x
                    [] -> Nothing

doubleField :: Monad monad => Field (GGWidget master monad ()) Double
doubleField = Field
    { fieldParse = maybe (Left "Invalid number") Right . readMay . unpack -- FIXME use Data.Text.Read
    , fieldRender = pack . show
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="text" :isReq:required="" value="#{val}">
|]
    }

dayField :: Monad monad => Field (GGWidget master monad ()) Day
dayField = Field
    { fieldParse = parseDate . unpack
    , fieldRender = pack . show
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="date" :isReq:required="" value="#{val}">
|]
    }

timeField :: Monad monad => Field (GGWidget master monad ()) TimeOfDay
timeField = Field
    { fieldParse = parseTime . unpack
    , fieldRender = pack . show . roundFullSeconds
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" :isReq:required="" value="#{val}">
|]
    }
  where
    roundFullSeconds tod =
        TimeOfDay (todHour tod) (todMin tod) fullSec
      where
        fullSec = fromInteger $ floor $ todSec tod

htmlField :: Monad monad => Field (GGWidget master monad ()) Html
htmlField = Field
    { fieldParse = Right . preEscapedString . sanitizeBalance . unpack -- FIXME make changes to xss-sanitize
    , fieldRender = pack . renderHtml
    , fieldView = \theId name val _isReq -> addHamlet
        [HAMLET|\
<textarea id="#{theId}" name="#{name}" .html>#{val}
|]
    }

-- | A newtype wrapper around a 'String' that converts newlines to HTML
-- br-tags.
newtype Textarea = Textarea { unTextarea :: Text }
    deriving (Show, Read, Eq, PersistField)
instance ToHtml Textarea where
    toHtml =
        unsafeByteString
        . S.concat
        . L.toChunks
        . toLazyByteString
        . fromWriteList writeHtmlEscapedChar
        . unpack
        . unTextarea
      where
        -- Taken from blaze-builder and modified with newline handling.
        writeHtmlEscapedChar '\n' = writeByteString "<br>"
        writeHtmlEscapedChar c    = B.writeHtmlEscapedChar c

textareaField :: Monad monad => Field (GGWidget master monad ()) Textarea
textareaField = Field
    { fieldParse = Right . Textarea
    , fieldRender = unTextarea
    , fieldView = \theId name val _isReq -> addHamlet
        [HAMLET|\
<textarea id="#{theId}" name="#{name}">#{val}
|]
    }

hiddenField :: Monad monad => Field (GGWidget master monad ()) Text
hiddenField = Field
    { fieldParse = Right
    , fieldRender = id
    , fieldView = \theId name val _isReq -> addHamlet
        [HAMLET|\
<input type="hidden" id="#{theId}" name="#{name}" value="#{val}">
|]
    }

textField :: Monad monad => Field (GGWidget master monad ()) Text
textField = Field
    { fieldParse = Right
    , fieldRender = id
    , fieldView = \theId name val isReq ->
        [WHAMLET|
<input id="#{theId}" name="#{name}" type="text" :isReq:required value="#{val}">
|]
    }

passwordField :: Monad monad => Field (GGWidget master monad ()) Text
passwordField = Field
    { fieldParse = Right
    , fieldRender = id
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="password" :isReq:required="" value="#{val}">
|]
    }

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

parseDate :: String -> Either Text Day
parseDate = maybe (Left "Invalid day, must be in YYYY-MM-DD format") Right
              . readMay . replace '/' '-'

-- | Replaces all instances of a value in a list by another value.
-- from http://hackage.haskell.org/packages/archive/cgi/3001.1.7.1/doc/html/src/Network-CGI-Protocol.html#replace
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

parseTime :: String -> Either Text TimeOfDay
parseTime (h2:':':m1:m2:[]) = parseTimeHelper ('0', h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:[]) = parseTimeHelper (h1, h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:' ':'A':'M':[]) =
    parseTimeHelper (h1, h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:' ':'P':'M':[]) =
    let [h1', h2'] = show $ (read [h1, h2] :: Int) + 12
    in parseTimeHelper (h1', h2', m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:':':s1:s2:[]) =
    parseTimeHelper (h1, h2, m1, m2, s1, s2)
parseTime _ = Left "Invalid time, must be in HH:MM[:SS] format"

parseTimeHelper :: (Char, Char, Char, Char, Char, Char)
                -> Either Text TimeOfDay
parseTimeHelper (h1, h2, m1, m2, s1, s2)
    | h < 0 || h > 23 = Left $ pack $ "Invalid hour: " ++ show h
    | m < 0 || m > 59 = Left $ pack $ "Invalid minute: " ++ show m
    | s < 0 || s > 59 = Left $ pack $ "Invalid second: " ++ show s
    | otherwise = Right $ TimeOfDay h m s
  where
    h = read [h1, h2]
    m = read [m1, m2]
    s = fromInteger $ read [s1, s2]

emailField :: Monad monad => Field (GGWidget master monad ()) Text
emailField = Field
    { fieldParse = \s -> if Email.isValid (unpack s)
                        then Right s
                        else Left "Invalid e-mail address"
    , fieldRender = id
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="email" :isReq:required="" value="#{val}">
|]
    }

type AutoFocus = Bool
searchField :: Monad monad => AutoFocus -> Field (GGWidget master monad ()) Text
searchField autoFocus = Field
    { fieldParse = Right
    , fieldRender = id
    , fieldView = \theId name val isReq -> do
        addHtml [HAMLET|\
<input id="#{theId}" name="#{name}" type="search" :isReq:required="" :autoFocus:autofocus="" value="#{val}">
|]
        when autoFocus $ do
          addHtml $ [HAMLET|\<script>if (!('autofocus' in document.createElement('input'))) {document.getElementById('#{theId}').focus();}</script> 
|]
          addCassius [CASSIUS|
            #{theId}
              -webkit-appearance: textfield
            |]
    }

urlField :: Monad monad => Field (GGWidget master monad ()) Text
urlField = Field
    { fieldParse = \s -> case parseURI $ unpack s of
                        Nothing -> Left "Invalid URL"
                        Just _ -> Right s
    , fieldRender = id
    , fieldView = \theId name val isReq -> addHtml
        [HAMLET|
<input ##{theId} name=#{name} type=url :isReq:required value=#{val}>
|]
    }
