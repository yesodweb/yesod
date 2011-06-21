{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Form.Fields
    ( FormMessage (..)
    , defaultFormMessage
    , textField
    , passwordField
    , textareaField
    , hiddenField
    , intField
    , dayField
    , timeField
    , htmlField
    , emailField
    , searchField
    , selectField
    , AutoFocus
    , urlField
    , doubleField
    , parseDate
    , parseTime
    , Textarea (..)
    , radioField
    , boolField
    ) where

import Yesod.Form.Types
import Yesod.Widget
import Yesod.Message (RenderMessage)
import Yesod.Handler (GGHandler)
import Text.Hamlet hiding (renderHtml)
import Text.Blaze (ToHtml (..))
import Text.Cassius
import Data.Time (Day, TimeOfDay(..))
import qualified Text.Email.Validate as Email
import Network.URI (parseURI)
import Database.Persist (PersistField)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Control.Monad (when, unless)

import qualified Blaze.ByteString.Builder.Html.Utf8 as B
import Blaze.ByteString.Builder (writeByteString, toLazyByteString)
import Blaze.ByteString.Builder.Internal.Write (fromWriteList)

import Text.Blaze.Renderer.String (renderHtml)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Read
import Data.Monoid (mappend)
import Text.Hamlet.NonPoly (html)

#if __GLASGOW_HASKELL__ >= 700
#define WHAMLET whamlet
#define HAMLET hamlet
#define CASSIUS cassius
#define JULIUS julius
#define HTML html
#else
#define WHAMLET $whamlet
#define HAMLET $hamlet
#define CASSIUS $cassius
#define JULIUS $julius
#define HTML $html
#endif

data FormMessage = MsgInvalidInteger Text
                 | MsgInvalidNumber Text
                 | MsgInvalidEntry Text
                 | MsgInvalidUrl Text
                 | MsgInvalidEmail Text
                 | MsgInvalidTimeFormat
                 | MsgInvalidHour Text
                 | MsgInvalidMinute Text
                 | MsgInvalidSecond Text
                 | MsgInvalidDay
                 | MsgCsrfWarning
                 | MsgValueRequired
                 | MsgInputNotFound Text
                 | MsgSelectNone
                 | MsgInvalidBool Text
                 | MsgBoolYes
                 | MsgBoolNo

defaultFormMessage :: FormMessage -> Text
defaultFormMessage (MsgInvalidInteger t) = "Invalid integer: " `mappend` t
defaultFormMessage (MsgInvalidNumber t) = "Invalid number: " `mappend` t
defaultFormMessage (MsgInvalidEntry t) = "Invalid entry: " `mappend` t
defaultFormMessage MsgInvalidTimeFormat = "Invalid time, must be in HH:MM[:SS] format"
defaultFormMessage MsgInvalidDay = "Invalid day, must be in YYYY-MM-DD format"
defaultFormMessage (MsgInvalidUrl t) = "Invalid URL: " `mappend` t
defaultFormMessage (MsgInvalidEmail t) = "Invalid e-mail address: " `mappend` t
defaultFormMessage (MsgInvalidHour t) = "Invalid hour: " `mappend` t
defaultFormMessage (MsgInvalidMinute t) = "Invalid minute: " `mappend` t
defaultFormMessage (MsgInvalidSecond t) = "Invalid second: " `mappend` t
defaultFormMessage MsgCsrfWarning = "As a protection against cross-site request forgery attacks, please confirm your form submission."
defaultFormMessage MsgValueRequired = "Value is required"
defaultFormMessage (MsgInputNotFound t) = "Input not found: " `mappend` t
defaultFormMessage MsgSelectNone = "<None>"
defaultFormMessage (MsgInvalidBool t) = "Invalid boolean: " `mappend` t
defaultFormMessage MsgBoolYes = "Yes"
defaultFormMessage MsgBoolNo = "No"

blank :: (Text -> Either msg a) -> [Text] -> Either msg (Maybe a)
blank _ [] = Right Nothing
blank _ ("":_) = Right Nothing
blank f (x:_) = either Left (Right . Just) $ f x



intField :: (Monad monad, Integral i) => Field (GGWidget master monad ()) FormMessage i
intField = Field
    { fieldParse = blank $ \s ->
        case Data.Text.Read.signed Data.Text.Read.decimal s of
            Right (a, "") -> Right a
            _ -> Left $ MsgInvalidInteger s

    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="number" :isReq:required="" value="#{showVal val}">
|]
    }
  where
    showVal = maybe "" (pack . showI)
    showI x = show (fromIntegral x :: Integer)

doubleField :: Monad monad => Field (GGWidget master monad ()) FormMessage Double
doubleField = Field
    { fieldParse = blank $ \s ->
        case Data.Text.Read.double s of
            Right (a, "") -> Right a
            _ -> Left $ MsgInvalidNumber s

    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="text" :isReq:required="" value="#{showVal val}">
|]
    }
  where showVal = maybe "" (pack . show)

dayField :: Monad monad => Field (GGWidget master monad ()) FormMessage Day
dayField = Field
    { fieldParse = blank $ parseDate . unpack
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="date" :isReq:required="" value="#{showVal val}">
|]
    }
  where showVal = maybe "" (pack . show)

timeField :: Monad monad => Field (GGWidget master monad ()) FormMessage TimeOfDay
timeField = Field
    { fieldParse = blank $ parseTime . unpack
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" :isReq:required="" value="#{showVal val}">
|]
    }
  where
    showVal = maybe "" (pack . show . roundFullSeconds)
    roundFullSeconds tod =
        TimeOfDay (todHour tod) (todMin tod) fullSec
      where
        fullSec = fromInteger $ floor $ todSec tod

htmlField :: Monad monad => Field (GGWidget master monad ()) FormMessage Html
htmlField = Field
    { fieldParse = blank $ Right . preEscapedString . sanitizeBalance . unpack -- FIXME make changes to xss-sanitize
    , fieldView = \theId name val _isReq -> addHamlet
        [HAMLET|\
<textarea id="#{theId}" name="#{name}" .html>#{showVal val}
|]
    }
  where showVal = maybe "" (pack . renderHtml)

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

textareaField :: Monad monad => Field (GGWidget master monad ()) FormMessage Textarea
textareaField = Field
    { fieldParse =  blank $ Right . Textarea
    , fieldView = \theId name val _isReq -> addHamlet
        [HAMLET|\
<textarea id="#{theId}" name="#{name}">#{maybe "" unTextarea val}
|]
    }

hiddenField :: Monad monad => Field (GGWidget master monad ()) FormMessage Text
hiddenField = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name val _isReq -> addHamlet
        [HAMLET|\
<input type="hidden" id="#{theId}" name="#{name}" value="#{maybe "" id val}">
|]
    }

textField :: Monad monad => Field (GGWidget master monad ()) FormMessage Text
textField = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name val isReq ->
        [WHAMLET|
<input id="#{theId}" name="#{name}" type="text" :isReq:required value="#{maybe "" id val}">
|]
    }

passwordField :: Monad monad => Field (GGWidget master monad ()) FormMessage Text
passwordField = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="password" :isReq:required="" value="#{maybe "" id val}">
|]
    }

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

parseDate :: String -> Either FormMessage Day
parseDate = maybe (Left MsgInvalidDay) Right
              . readMay . replace '/' '-'

-- | Replaces all instances of a value in a list by another value.
-- from http://hackage.haskell.org/packages/archive/cgi/3001.1.7.1/doc/html/src/Network-CGI-Protocol.html#replace
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

parseTime :: String -> Either FormMessage TimeOfDay
parseTime (h2:':':m1:m2:[]) = parseTimeHelper ('0', h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:[]) = parseTimeHelper (h1, h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:' ':'A':'M':[]) =
    parseTimeHelper (h1, h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:' ':'P':'M':[]) =
    let [h1', h2'] = show $ (read [h1, h2] :: Int) + 12
    in parseTimeHelper (h1', h2', m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:':':s1:s2:[]) =
    parseTimeHelper (h1, h2, m1, m2, s1, s2)
parseTime _ = Left MsgInvalidTimeFormat

parseTimeHelper :: (Char, Char, Char, Char, Char, Char)
                -> Either FormMessage TimeOfDay
parseTimeHelper (h1, h2, m1, m2, s1, s2)
    | h < 0 || h > 23 = Left $ MsgInvalidHour $ pack [h1, h2]
    | m < 0 || m > 59 = Left $ MsgInvalidMinute $ pack [m1, m2]
    | s < 0 || s > 59 = Left $ MsgInvalidSecond $ pack [s1, s2]
    | otherwise = Right $ TimeOfDay h m s
  where
    h = read [h1, h2] -- FIXME isn't this a really bad idea?
    m = read [m1, m2]
    s = fromInteger $ read [s1, s2]

emailField :: Monad monad => Field (GGWidget master monad ()) FormMessage Text
emailField = Field
    { fieldParse = blank $
        \s -> if Email.isValid (unpack s)
                then Right s
                else Left $ MsgInvalidEmail s
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="email" :isReq:required="" value="#{maybe "" id val}">
|]
    }

type AutoFocus = Bool
searchField :: Monad monad => AutoFocus -> Field (GGWidget master monad ()) FormMessage Text
searchField autoFocus = Field
    { fieldParse =  blank Right
    , fieldView = \theId name val isReq -> do
        addHtml [HAMLET|\
<input id="#{theId}" name="#{name}" type="search" :isReq:required="" :autoFocus:autofocus="" value="#{maybe "" id val}">
|]
        when autoFocus $ do
          addHtml $ [HAMLET|\<script>if (!('autofocus' in document.createElement('input'))) {document.getElementById('#{theId}').focus();}</script> 
|]
          addCassius [CASSIUS|
            #{theId}
              -webkit-appearance: textfield
            |]
    }

urlField :: Monad monad => Field (GGWidget master monad ()) FormMessage Text
urlField = Field
    { fieldParse = blank $ \s ->
        case parseURI $ unpack s of
            Nothing -> Left $ MsgInvalidUrl s
            Just _ -> Right s
    , fieldView = \theId name val isReq -> addHtml
        [HAMLET|
<input ##{theId} name=#{name} type=url :isReq:required value=#{maybe "" id val}>
|]
    }

selectField :: (Eq a, Monad monad, RenderMessage master FormMessage) => [(Text, a)] -> Field (GGWidget master (GGHandler sub master monad) ()) FormMessage a
selectField = selectFieldHelper
    (\theId name inside -> [WHAMLET|<select ##{theId} name=#{name}>^{inside}|])
    (\_theId _name isSel -> [WHAMLET|<option value=none :isSel:selected>_{MsgSelectNone}|])
    (\_theId _name value isSel text -> addHtml [HTML|<option value=#{value} :isSel:selected>#{text}|])

radioField :: (Eq a, Monad monad, RenderMessage master FormMessage) => [(Text, a)] -> Field (GGWidget master (GGHandler sub master monad) ()) FormMessage a
radioField = selectFieldHelper
    (\theId _name inside -> [WHAMLET|<div ##{theId}>^{inside}|])
    (\theId name isSel -> [WHAMLET|
<div>
    <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
    <label for=#{theId}-none>_{MsgSelectNone}
|])
    (\theId name value isSel text -> [WHAMLET|
<div>
    <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked>
    <label for=#{theId}-#{value}>#{text}
|])

boolField :: (Monad monad, RenderMessage master FormMessage) => Field (GGWidget master (GGHandler sub master monad) ()) FormMessage Bool
boolField = Field
      { fieldParse = boolParser
      , fieldView = \theId name val isReq -> [WHAMLET|
  $if not isReq
      <input id=#{theId}-none type=radio name=#{name} value=none checked>
      <label for=#{theId}-none>_{MsgSelectNone}


<input id=#{theId}-yes type=radio name=#{name} value=yes :maybe False id val:checked>
<label for=#{theId}-yes>_{MsgBoolYes}

<input id=#{theId}-no type=radio name=#{name} value=no :maybe False not val:checked>
<label for=#{theId}-no>_{MsgBoolNo}
|]
    }
  where
    boolParser [] = Right Nothing
    boolParser (x:_) = case x of
      "" -> Right Nothing
      "none" -> Right Nothing
      "yes" -> Right $ Just True
      "no" -> Right $ Just False
      t -> Left $ MsgInvalidBool t


selectFieldHelper :: (Eq a, Monad monad)
        => (Text -> Text -> GGWidget master monad () -> GGWidget master monad ())
        -> (Text -> Text -> Bool -> GGWidget master monad ())
        -> (Text -> Text -> Text -> Bool -> Text -> GGWidget master monad ())
        -> [(Text, a)] -> Field (GGWidget master monad ()) FormMessage a
selectFieldHelper outside onOpt inside opts = Field
    { fieldParse = selectParser
    , fieldView = \theId name val isReq ->
        outside theId name $ do
            unless isReq $ onOpt theId name $ not $ (render val) `elem` map (pack . show . fst) pairs
            flip mapM_ pairs $ \pair -> inside
                theId
                name
                (pack $ show $ fst pair)
                ((render val) == pack (show $ fst pair))
                (fst $ snd pair)
    }
  where
    pairs = zip [1 :: Int ..] opts -- FIXME use IntMap
    rpairs = zip (map snd opts) [1 :: Int ..]
    render Nothing = ""
    render (Just a) = maybe "" (pack . show) $ lookup a rpairs
    selectParser [] = Right Nothing
    selectParser (s:_) = case s of
            "" -> Right Nothing
            "none" -> Right Nothing
            x -> case Data.Text.Read.decimal x of
                    Right (a, "") ->
                        case lookup a pairs of
                            Nothing -> Left $ MsgInvalidEntry x
                            Just y -> Right $ Just $ snd y
                    _ -> Left $ MsgInvalidNumber x
