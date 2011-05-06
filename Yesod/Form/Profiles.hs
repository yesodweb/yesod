{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Yesod.Form.Profiles
    ( stringFieldProfile
    , passwordFieldProfile
    , textareaFieldProfile
    , hiddenFieldProfile
    , intFieldProfile
    , dayFieldProfile
    , timeFieldProfile
    , htmlFieldProfile
    , emailFieldProfile
    , searchFieldProfile
    , AutoFocus
    , urlFieldProfile
    , doubleFieldProfile
    , parseDate
    , parseTime
    , Textarea (..)
    ) where

import Yesod.Form.Core
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
#define HAMLET hamlet
#define CASSIUS cassius
#define JULIUS julius
#else
#define HAMLET $hamlet
#define CASSIUS $cassius
#define JULIUS $julius
#endif

intFieldProfile :: (Monad monad, Integral i) => FieldProfile (GGWidget master monad ()) i
intFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid integer") Right . readMayI . unpack -- FIXME Data.Text.Read
    , fpRender = pack . showI
    , fpWidget = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="number" :isReq:required="" value="#{val}">
|]
    }
  where
    showI x = show (fromIntegral x :: Integer)
    readMayI s = case reads s of
                    (x, _):_ -> Just $ fromInteger x
                    [] -> Nothing

doubleFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) Double
doubleFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid number") Right . readMay . unpack -- FIXME use Data.Text.Read
    , fpRender = pack . show
    , fpWidget = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="text" :isReq:required="" value="#{val}">
|]
    }

dayFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) Day
dayFieldProfile = FieldProfile
    { fpParse = parseDate . unpack
    , fpRender = pack . show
    , fpWidget = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="date" :isReq:required="" value="#{val}">
|]
    }

timeFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) TimeOfDay
timeFieldProfile = FieldProfile
    { fpParse = parseTime . unpack
    , fpRender = pack . show . roundFullSeconds
    , fpWidget = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" :isReq:required="" value="#{val}">
|]
    }
  where
    roundFullSeconds tod =
        TimeOfDay (todHour tod) (todMin tod) fullSec
      where
        fullSec = fromInteger $ floor $ todSec tod

htmlFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) Html
htmlFieldProfile = FieldProfile
    { fpParse = Right . preEscapedString . sanitizeBalance . unpack -- FIXME make changes to xss-sanitize
    , fpRender = pack . renderHtml
    , fpWidget = \theId name val _isReq -> addHamlet
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

textareaFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) Textarea
textareaFieldProfile = FieldProfile
    { fpParse = Right . Textarea
    , fpRender = unTextarea
    , fpWidget = \theId name val _isReq -> addHamlet
        [HAMLET|\
<textarea id="#{theId}" name="#{name}">#{val}
|]
    }

hiddenFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) Text
hiddenFieldProfile = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpWidget = \theId name val _isReq -> addHamlet
        [HAMLET|\
<input type="hidden" id="#{theId}" name="#{name}" value="#{val}">
|]
    }

stringFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) Text
stringFieldProfile = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpWidget = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="text" :isReq:required="" value="#{val}">
|]
    }

passwordFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) Text
passwordFieldProfile = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpWidget = \theId name val isReq -> addHamlet
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

emailFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) Text
emailFieldProfile = FieldProfile
    { fpParse = \s -> if Email.isValid (unpack s)
                        then Right s
                        else Left "Invalid e-mail address"
    , fpRender = id
    , fpWidget = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="email" :isReq:required="" value="#{val}">
|]
    }

type AutoFocus = Bool
searchFieldProfile :: Monad monad => AutoFocus -> FieldProfile (GGWidget master monad ()) Text
searchFieldProfile autoFocus = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpWidget = \theId name val isReq -> do
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

urlFieldProfile :: Monad monad => FieldProfile (GGWidget master monad ()) Text
urlFieldProfile = FieldProfile
    { fpParse = \s -> case parseURI $ unpack s of
                        Nothing -> Left "Invalid URL"
                        Just _ -> Right s
    , fpRender = id
    , fpWidget = \theId name val isReq -> addHtml
        [HAMLET|
<input ##{theId} name=#{name} type=url :isReq:required value=#{val}>
|]
    }
