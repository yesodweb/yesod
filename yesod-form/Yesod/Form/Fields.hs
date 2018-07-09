{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | Field functions allow you to easily create and validate forms, cleanly handling the uncertainty of parsing user input.
--
-- When possible, the field functions use a specific input type (e.g. "number"), allowing supporting browsers to validate the input before form submission. Browsers can also improve usability with this information; for example, mobile browsers might present a specialized keyboard for an input of type "email" or "number".
--
-- See the Yesod book <http://www.yesodweb.com/book/forms chapter on forms> for a broader overview of forms in Yesod.
module Yesod.Form.Fields
    ( -- * i18n
      FormMessage (..)
    , defaultFormMessage
      -- * Fields
    , textField
    , passwordField
    , textareaField
    , hiddenField
    , intField
    , dayField
    , timeField
    , timeFieldTypeTime
    , timeFieldTypeText
    , htmlField
    , emailField
    , multiEmailField
    , searchField
    , AutoFocus
    , urlField
    , doubleField
    , parseDate
    , parseTime
    , Textarea (..)
    , boolField
    , checkBoxField
    , fileField
      -- * File 'AForm's
    , fileAFormReq
    , fileAFormOpt
      -- * Options
      -- $optionsOverview
    , selectFieldHelper
    , selectField
    , selectFieldList
    , radioField
    , radioFieldList
    , checkboxesField
    , checkboxesFieldList
    , multiSelectField
    , multiSelectFieldList
    , Option (..)
    , OptionList (..)
    , mkOptionList
    , optionsPersist
    , optionsPersistKey
    , optionsPairs
    , optionsEnum
    ) where

import Yesod.Form.Types
import Yesod.Form.I18n.English
import Yesod.Form.Functions (parseHelper)
import Yesod.Core
import Text.Blaze (ToMarkup (toMarkup), unsafeByteString)
#define ToHtml ToMarkup
#define toHtml toMarkup
#define preEscapedText preEscapedToMarkup
import Data.Time (Day, TimeOfDay(..))
import qualified Text.Email.Validate as Email
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.URI (parseURI)
import Database.Persist.Sql (PersistField, PersistFieldSql (..))
#if MIN_VERSION_persistent(2,5,0)
import Database.Persist (Entity (..), SqlType (SqlString), PersistRecordBackend, PersistQueryRead)
#else
import Database.Persist (Entity (..), SqlType (SqlString), PersistEntity, PersistQuery, PersistEntityBackend)
#endif
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Control.Monad (when, unless)
import Data.Either (partitionEithers)
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Blaze.ByteString.Builder.Html.Utf8 as B
import Blaze.ByteString.Builder (writeByteString, toLazyByteString)
import Blaze.ByteString.Builder.Internal.Write (fromWriteList)

import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text as T ( Text, append, concat, cons, head
                      , intercalate, isPrefixOf, null, unpack, pack, splitOn
                      )
import qualified Data.Text as T (drop, dropWhile)
import qualified Data.Text.Read

import qualified Data.Map as Map
import Yesod.Persist (selectList, Filter, SelectOpt, Key)
import Control.Arrow ((&&&))

import Control.Applicative ((<$>), (<|>))

import Data.Attoparsec.Text (Parser, char, string, digit, skipSpace, endOfInput, parseOnly)

import Yesod.Persist.Core

import Data.String (IsString)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

defaultFormMessage :: FormMessage -> Text
defaultFormMessage = englishFormMessage

-- | Creates a input with @type="number"@ and @step=1@.
intField :: (Monad m, Integral i, RenderMessage (HandlerSite m) FormMessage) => Field m i
intField = Field
    { fieldParse = parseHelper $ \s ->
        case Data.Text.Read.signed Data.Text.Read.decimal s of
            Right (a, "") -> Right a
            _ -> Left $ MsgInvalidInteger s

    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="number" step=1 :isReq:required="" value="#{showVal val}">
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)

-- | Creates a input with @type="number"@ and @step=any@.
doubleField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Double
doubleField = Field
    { fieldParse = parseHelper $ \s ->
        case Data.Text.Read.double (prependZero s) of
            Right (a, "") -> Right a
            _ -> Left $ MsgInvalidNumber s

    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="number" step=any :isReq:required="" value="#{showVal val}">
|]
    , fieldEnctype = UrlEncoded
    }
  where showVal = either id (pack . show)

-- | Creates an input with @type="date"@, validating the input using the 'parseDate' function.
--
-- Add the @time@ package and import the "Data.Time.Calendar" module to use this function.
dayField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Day
dayField = Field
    { fieldParse = parseHelper $ parseDate . unpack
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="date" :isReq:required="" value="#{showVal val}">
|]
    , fieldEnctype = UrlEncoded
    }
  where showVal = either id (pack . show)

-- | An alias for 'timeFieldTypeTime'.
timeField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m TimeOfDay
timeField = timeFieldTypeTime

-- | Creates an input with @type="time"@. <http://caniuse.com/#search=time%20input%20type Browsers not supporting this type> will fallback to a text field, and Yesod will parse the time as described in 'timeFieldTypeText'.
-- 
-- Add the @time@ package and import the "Data.Time.LocalTime" module to use this function.
--
-- Since 1.4.2
timeFieldTypeTime :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m TimeOfDay  
timeFieldTypeTime = timeFieldOfType "time"

-- | Creates an input with @type="text"@, parsing the time from an [H]H:MM[:SS] format, with an optional AM or PM (if not given, AM is assumed for compatibility with the 24 hour clock system).
--
-- This function exists for backwards compatibility with the old implementation of 'timeField', which used to use @type="text"@. Consider using 'timeField' or 'timeFieldTypeTime' for improved UX and validation from the browser.
-- 
-- Add the @time@ package and import the "Data.Time.LocalTime" module to use this function.
--
-- Since 1.4.2
timeFieldTypeText :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m TimeOfDay
timeFieldTypeText = timeFieldOfType "text"

timeFieldOfType :: Monad m => RenderMessage (HandlerSite m) FormMessage => Text -> Field m TimeOfDay
timeFieldOfType inputType = Field
    { fieldParse = parseHelper parseTime
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="#{inputType}" :isReq:required="" value="#{showVal val}">
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . show . roundFullSeconds)
    roundFullSeconds tod =
        TimeOfDay (todHour tod) (todMin tod) fullSec
      where
        fullSec = fromInteger $ floor $ todSec tod

-- | Creates a @\<textarea>@ tag whose input is sanitized to prevent XSS attacks and is validated for having balanced tags.
htmlField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Html
htmlField = Field
    { fieldParse = parseHelper $ Right . preEscapedText . sanitizeBalance
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<textarea :isReq:required="" id="#{theId}" name="#{name}" *{attrs}>#{showVal val}
|]
    , fieldEnctype = UrlEncoded
    }
  where showVal = either id (pack . renderHtml)

-- | A newtype wrapper around a 'Text' whose 'ToMarkup' instance converts newlines to HTML @\<br>@ tags.
-- 
-- (When text is entered into a @\<textarea>@, newline characters are used to separate lines.
-- If this text is then placed verbatim into HTML, the lines won't be separated, thus the need for replacing with @\<br>@ tags).
-- If you don't need this functionality, simply use 'unTextarea' to access the raw text.
newtype Textarea = Textarea { unTextarea :: Text }
    deriving (Show, Read, Eq, PersistField, Ord, ToJSON, FromJSON, IsString)
instance PersistFieldSql Textarea where
    sqlType _ = SqlString
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
        writeHtmlEscapedChar '\r' = mempty
        writeHtmlEscapedChar '\n' = writeByteString "<br>"
        writeHtmlEscapedChar c    = B.writeHtmlEscapedChar c

-- | Creates a @\<textarea>@ tag whose returned value is wrapped in a 'Textarea'; see 'Textarea' for details.
textareaField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Textarea
textareaField = Field
    { fieldParse = parseHelper $ Right . Textarea
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<textarea id="#{theId}" name="#{name}" :isReq:required="" *{attrs}>#{either id unTextarea val}
|]
    , fieldEnctype = UrlEncoded
    }

-- | Creates an input with @type="hidden"@; you can use this to store information in a form that users shouldn't see (for example, Yesod stores CSRF tokens in a hidden field).
hiddenField :: (Monad m, PathPiece p, RenderMessage (HandlerSite m) FormMessage)
            => Field m p
hiddenField = Field
    { fieldParse = parseHelper $ maybe (Left MsgValueRequired) Right . fromPathPiece
    , fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
$newline never
<input type="hidden" id="#{theId}" name="#{name}" *{attrs} value="#{either id toPathPiece val}">
|]
    , fieldEnctype = UrlEncoded
    }

-- | Creates a input with @type="text"@.
textField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
textField = Field
    { fieldParse = parseHelper $ Right
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either id id val}">
|]
    , fieldEnctype = UrlEncoded
    }
-- | Creates an input with @type="password"@.
passwordField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
passwordField = Field
    { fieldParse = parseHelper $ Right
    , fieldView = \theId name attrs _ isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="password" :isReq:required="">
|]
    , fieldEnctype = UrlEncoded
    }

readMay :: Read a => String -> Maybe a
readMay s = case filter (Prelude.null . snd) $ reads s of
                (x, _):_ -> Just x
                [] -> Nothing

-- | Parses a 'Day' from a 'String'.
parseDate :: String -> Either FormMessage Day
parseDate = maybe (Left MsgInvalidDay) Right
              . readMay . replace '/' '-'

-- | Replaces all instances of a value in a list by another value.
-- from http://hackage.haskell.org/packages/archive/cgi/3001.1.7.1/doc/html/src/Network-CGI-Protocol.html#replace
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

parseTime :: Text -> Either FormMessage TimeOfDay
parseTime = either (Left . fromMaybe MsgInvalidTimeFormat . readMay . drop 2 . dropWhile (/= ':')) Right . parseOnly timeParser

timeParser :: Parser TimeOfDay
timeParser = do
    skipSpace
    h <- hour
    _ <- char ':'
    m <- minsec MsgInvalidMinute
    hasSec <- (char ':' >> return True) <|> return False
    s <- if hasSec then minsec MsgInvalidSecond else return 0
    skipSpace
    isPM <-
        (string "am" >> return (Just False)) <|>
        (string "AM" >> return (Just False)) <|>
        (string "pm" >> return (Just True)) <|>
        (string "PM" >> return (Just True)) <|>
        return Nothing
    h' <-
        case isPM of
            Nothing -> return h
            Just x
                | h <= 0 || h > 12 -> fail $ show $ MsgInvalidHour $ pack $ show h
                | h == 12 -> return $ if x then 12 else 0
                | otherwise -> return $ h + (if x then 12 else 0)
    skipSpace
    endOfInput
    return $ TimeOfDay h' m s
  where
    hour = do
        x <- digit
        y <- (return Control.Applicative.<$> digit) <|> return []
        let xy = x : y
        let i = read xy
        if i < 0 || i >= 24
            then fail $ show $ MsgInvalidHour $ pack xy
            else return i
    minsec :: Num a => (Text -> FormMessage) -> Parser a
    minsec msg = do
        x <- digit
        y <- digit <|> fail (show $ msg $ pack [x])
        let xy = [x, y]
        let i = read xy
        if i < 0 || i >= 60
            then fail $ show $ msg $ pack xy
            else return $ fromIntegral (i :: Int)
            
-- | Creates an input with @type="email"@. Yesod will validate the email's correctness according to RFC5322 and canonicalize it by removing comments and whitespace (see "Text.Email.Validate").
emailField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
emailField = Field
    { fieldParse = parseHelper $
        \s ->
            case Email.canonicalizeEmail $ encodeUtf8 s of
                Just e -> Right $ decodeUtf8With lenientDecode e
                Nothing -> Left $ MsgInvalidEmail s
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="email" :isReq:required="" value="#{either id id val}">
|]
    , fieldEnctype = UrlEncoded
    }

-- | Creates an input with @type="email"@ with the <http://w3c.github.io/html/sec-forms.html#the-multiple-attribute multiple> attribute; browsers might implement this as taking a comma separated list of emails. Each email address is validated as described in 'emailField'.
--
-- Since 1.3.7
multiEmailField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m [Text]
multiEmailField = Field
    { fieldParse = parseHelper $
        \s ->
            let addrs = map validate $ splitOn "," s
            in case partitionEithers addrs of
                ([], good) -> Right good
                (bad, _) -> Left $ MsgInvalidEmail $ cat bad
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="email" multiple :isReq:required="" value="#{either id cat val}">
|]
    , fieldEnctype = UrlEncoded
    }
    where
        -- report offending address along with error
        validate a = case Email.validate $ encodeUtf8 a of
                        Left e -> Left $ T.concat [a, " (",  pack e, ")"]
                        Right r -> Right $ emailToText r
        cat = intercalate ", "
        emailToText = decodeUtf8With lenientDecode . Email.toByteString

type AutoFocus = Bool
-- | Creates an input with @type="search"@. For <http://caniuse.com/#search=autofocus browsers without autofocus support>, a JS fallback is used if @AutoFocus@ is true.
searchField :: Monad m => RenderMessage (HandlerSite m) FormMessage => AutoFocus -> Field m Text
searchField autoFocus = Field
    { fieldParse = parseHelper Right
    , fieldView = \theId name attrs val isReq -> do
        [whamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="search" :isReq:required="" :autoFocus:autofocus="" value="#{either id id val}">
|]
        when autoFocus $ do
          -- we want this javascript to be placed immediately after the field
          [whamlet|
$newline never
<script>if (!('autofocus' in document.createElement('input'))) {document.getElementById('#{theId}').focus();}
|]
          toWidget [cassius|
            ##{theId}
              -webkit-appearance: textfield
            |]
    , fieldEnctype = UrlEncoded
    }
-- | Creates an input with @type="url"@, validating the URL according to RFC3986.
urlField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
urlField = Field
    { fieldParse = parseHelper $ \s ->
        case parseURI $ unpack s of
            Nothing -> Left $ MsgInvalidUrl s
            Just _ -> Right s
    , fieldView = \theId name attrs val isReq ->
        [whamlet|<input ##{theId} name=#{name} *{attrs} type=url :isReq:required value=#{either id id val}>|]
    , fieldEnctype = UrlEncoded
    }

-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectFieldList [("Value 1" :: Text, "value1"),("Value 2", "value2")]) "Which value?" Nothing
selectFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
                => [(msg, a)]
                -> Field (HandlerFor site) a
selectFieldList = selectField . optionsPairs

-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectField $ optionsPairs [(MsgValue1, "value1"),(MsgValue2, "value2")]) "Which value?" Nothing
selectField :: (Eq a, RenderMessage site FormMessage)
            => HandlerFor site (OptionList a)
            -> Field (HandlerFor site) a
selectField = selectFieldHelper
    (\theId name attrs inside -> [whamlet|
$newline never
<select ##{theId} name=#{name} *{attrs}>^{inside}
|]) -- outside
    (\_theId _name isSel -> [whamlet|
$newline never
<option value=none :isSel:selected>_{MsgSelectNone}
|]) -- onOpt
    (\_theId _name _attrs value isSel text -> [whamlet|
$newline never
<option value=#{value} :isSel:selected>#{text}
|]) -- inside

-- | Creates a @\<select>@ tag for selecting multiple options.
multiSelectFieldList :: (Eq a, RenderMessage site msg)
                     => [(msg, a)]
                     -> Field (HandlerFor site) [a]
multiSelectFieldList = multiSelectField . optionsPairs

-- | Creates a @\<select>@ tag for selecting multiple options.
multiSelectField :: Eq a
                 => HandlerFor site (OptionList a)
                 -> Field (HandlerFor site) [a]
multiSelectField ioptlist =
    Field parse view UrlEncoded
  where
    parse [] _ = return $ Right Nothing
    parse optlist _ = do
        mapopt <- olReadExternal <$> ioptlist
        case mapM mapopt optlist of
             Nothing -> return $ Left "Error parsing values"
             Just res -> return $ Right $ Just res

    view theId name attrs val isReq = do
        opts <- fmap olOptions $ handlerToWidget ioptlist
        let selOpts = map (id &&& (optselected val)) opts
        [whamlet|
            <select ##{theId} name=#{name} :isReq:required multiple *{attrs}>
                $forall (opt, optsel) <- selOpts
                    <option value=#{optionExternalValue opt} :optsel:selected>#{optionDisplay opt}
                |]
        where
            optselected (Left _) _ = False
            optselected (Right vals) opt = (optionInternalValue opt) `elem` vals

-- | Creates an input with @type="radio"@ for selecting one option.
radioFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
               => [(msg, a)]
               -> Field (HandlerFor site) a
radioFieldList = radioField . optionsPairs

-- | Creates an input with @type="checkbox"@ for selecting multiple options.
checkboxesFieldList :: (Eq a, RenderMessage site msg) => [(msg, a)]
                     -> Field (HandlerFor site) [a]
checkboxesFieldList = checkboxesField . optionsPairs

-- | Creates an input with @type="checkbox"@ for selecting multiple options.
checkboxesField :: Eq a
                 => HandlerFor site (OptionList a)
                 -> Field (HandlerFor site) [a]
checkboxesField ioptlist = (multiSelectField ioptlist)
    { fieldView =
        \theId name attrs val _isReq -> do
            opts <- fmap olOptions $ handlerToWidget ioptlist
            let optselected (Left _) _ = False
                optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
            [whamlet|
                <span ##{theId}>
                    $forall opt <- opts
                        <label>
                            <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                            #{optionDisplay opt}
                |]
    }
-- | Creates an input with @type="radio"@ for selecting one option.
radioField :: (Eq a, RenderMessage site FormMessage)
           => HandlerFor site (OptionList a)
           -> Field (HandlerFor site) a
radioField = selectFieldHelper
    (\theId _name _attrs inside -> [whamlet|
$newline never
<div ##{theId}>^{inside}
|])
    (\theId name isSel -> [whamlet|
$newline never
<label .radio for=#{theId}-none>
    <div>
        <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
        _{MsgSelectNone}
|])
    (\theId name attrs value isSel text -> [whamlet|
$newline never
<label .radio for=#{theId}-#{value}>
    <div>
        <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
        \#{text}
|])

-- | Creates a group of radio buttons to answer the question given in the message. Radio buttons are used to allow differentiating between an empty response (@Nothing@) and a no response (@Just False@). Consider using the simpler 'checkBoxField' if you don't need to make this distinction.
--
-- If this field is optional, the first radio button is labeled "\<None>", the second \"Yes" and the third \"No".
--
-- If this field is required, the first radio button is labeled \"Yes" and the second \"No". 
--
-- (Exact label titles will depend on localization).
boolField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Bool
boolField = Field
      { fieldParse = \e _ -> return $ boolParser e
      , fieldView = \theId name attrs val isReq -> [whamlet|
$newline never
  $if not isReq
      <input id=#{theId}-none *{attrs} type=radio name=#{name} value=none checked>
      <label for=#{theId}-none>_{MsgSelectNone}


<input id=#{theId}-yes *{attrs} type=radio name=#{name} value=yes :showVal id val:checked>
<label for=#{theId}-yes>_{MsgBoolYes}

<input id=#{theId}-no *{attrs} type=radio name=#{name} value=no :showVal not val:checked>
<label for=#{theId}-no>_{MsgBoolNo}
|]
    , fieldEnctype = UrlEncoded
    }
  where
    boolParser [] = Right Nothing
    boolParser (x:_) = case x of
      "" -> Right Nothing
      "none" -> Right Nothing
      "yes" -> Right $ Just True
      "on" -> Right $ Just True
      "no" -> Right $ Just False
      "true" -> Right $ Just True
      "false" -> Right $ Just False
      t -> Left $ SomeMessage $ MsgInvalidBool t
    showVal = either (\_ -> False)

-- | Creates an input with @type="checkbox"@. 
--   While the default @'boolField'@ implements a radio button so you
--   can differentiate between an empty response (@Nothing@) and a no
--   response (@Just False@), this simpler checkbox field returns an empty
--   response as @Just False@.
--
--   Note that this makes the field always optional.
--
checkBoxField :: Monad m => Field m Bool
checkBoxField = Field
    { fieldParse = \e _ -> return $ checkBoxParser e
    , fieldView  = \theId name attrs val _ -> [whamlet|
$newline never
<input id=#{theId} *{attrs} type=checkbox name=#{name} value=yes :showVal id val:checked>
|]
    , fieldEnctype = UrlEncoded
    }

    where
        checkBoxParser [] = Right $ Just False
        checkBoxParser (x:_) = case x of
            "yes" -> Right $ Just True
            "on" -> Right $ Just True
            _     -> Right $ Just False

        showVal = either (\_ -> False)

-- | A structure holding a list of options. Typically you can use a convenience function like 'mkOptionList' or 'optionsPairs' instead of creating this directly.
data OptionList a = OptionList
    { olOptions :: [Option a]
    , olReadExternal :: Text -> Maybe a -- ^ A function mapping from the form's value ('optionExternalValue') to the selected Haskell value ('optionInternalValue').
    }

-- | Since 1.4.6
instance Functor OptionList where
    fmap f (OptionList options readExternal) = 
      OptionList ((fmap.fmap) f options) (fmap f . readExternal)

-- | Creates an 'OptionList', using a 'Map' to implement the 'olReadExternal' function.
mkOptionList :: [Option a] -> OptionList a
mkOptionList os = OptionList
    { olOptions = os
    , olReadExternal = flip Map.lookup $ Map.fromList $ map (optionExternalValue &&& optionInternalValue) os
    }

data Option a = Option
    { optionDisplay :: Text -- ^ The user-facing label.
    , optionInternalValue :: a -- ^ The Haskell value being selected.
    , optionExternalValue :: Text -- ^ The representation of this value stored in the form.
    }

-- | Since 1.4.6
instance Functor Option where
    fmap f (Option display internal external) = Option display (f internal) external

-- | Creates an 'OptionList' from a list of (display-value, internal value) pairs.
optionsPairs :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
             => [(msg, a)] -> m (OptionList a)
optionsPairs opts = do
  mr <- getMessageRender
  let mkOption external (display, internal) =
          Option { optionDisplay       = mr display
                 , optionInternalValue = internal
                 , optionExternalValue = pack $ show external
                 }
  return $ mkOptionList (zipWith mkOption [1 :: Int ..] opts)

-- | Creates an 'OptionList' from an 'Enum', using its 'Show' instance for the user-facing value.
optionsEnum :: (MonadHandler m, Show a, Enum a, Bounded a) => m (OptionList a)
optionsEnum = optionsPairs $ map (\x -> (pack $ show x, x)) [minBound..maxBound]

-- | Selects a list of 'Entity's with the given 'Filter' and 'SelectOpt's. The @(a -> msg)@ function is then used to derive the display value for an 'OptionList'. Example usage:
--
-- > Country
-- >    name Text
-- >    deriving Eq -- Must derive Eq
--
-- > data CountryForm = CountryForm
-- >   { country :: Entity Country
-- >   }
-- >
-- > countryNameForm :: AForm Handler CountryForm
-- > countryNameForm = CountryForm
-- >         <$> areq (selectField countries) "Which country do you live in?" Nothing
-- >         where
-- >           countries = optionsPersist [] [Asc CountryName] countryName
#if MIN_VERSION_persistent(2,5,0)
optionsPersist :: ( YesodPersist site
                  , PersistQueryRead backend
                  , PathPiece (Key a)
                  , RenderMessage site msg
                  , YesodPersistBackend site ~ backend
                  , PersistRecordBackend a backend
                  )
               => [Filter a]
               -> [SelectOpt a]
               -> (a -> msg)
               -> HandlerFor site (OptionList (Entity a))
#else
optionsPersist :: ( YesodPersist site, PersistEntity a
                  , PersistQuery (PersistEntityBackend a)
                  , PathPiece (Key a)
                  , RenderMessage site msg
                  , YesodPersistBackend site ~ PersistEntityBackend a
                  )
               => [Filter a]
               -> [SelectOpt a]
               -> (a -> msg)
               -> HandlerFor site (OptionList (Entity a))
#endif
optionsPersist filts ords toDisplay = fmap mkOptionList $ do
    mr <- getMessageRender
    pairs <- runDB $ selectList filts ords
    return $ map (\(Entity key value) -> Option
        { optionDisplay = mr (toDisplay value)
        , optionInternalValue = Entity key value
        , optionExternalValue = toPathPiece key
        }) pairs

-- | An alternative to 'optionsPersist' which returns just the 'Key' instead of
-- the entire 'Entity'.
--
-- Since 1.3.2
#if MIN_VERSION_persistent(2,5,0)
optionsPersistKey
  :: (YesodPersist site
     , PersistQueryRead backend
     , PathPiece (Key a)
     , RenderMessage site msg
     , backend ~ YesodPersistBackend site
     , PersistRecordBackend a backend
     )
  => [Filter a]
  -> [SelectOpt a]
  -> (a -> msg)
  -> HandlerFor site (OptionList (Key a))
#else
optionsPersistKey
  :: (YesodPersist site
     , PersistEntity a
     , PersistQuery (PersistEntityBackend a)
     , PathPiece (Key a)
     , RenderMessage site msg
     , YesodPersistBackend site ~ PersistEntityBackend a
     )
  => [Filter a]
  -> [SelectOpt a]
  -> (a -> msg)
  -> HandlerFor site (OptionList (Key a))
#endif

optionsPersistKey filts ords toDisplay = fmap mkOptionList $ do
    mr <- getMessageRender
    pairs <- runDB $ selectList filts ords
    return $ map (\(Entity key value) -> Option
        { optionDisplay = mr (toDisplay value)
        , optionInternalValue = key
        , optionExternalValue = toPathPiece key
        }) pairs

-- |
-- A helper function for constucting 'selectField's. You may want to use this when you define your custom 'selectField's or 'radioField's.
--
-- @since 1.6.2
selectFieldHelper
        :: (Eq a, RenderMessage site FormMessage)
        => (Text -> Text -> [(Text, Text)] -> WidgetFor site () -> WidgetFor site ()) -- ^ Outermost part of the field
        -> (Text -> Text -> Bool -> WidgetFor site ()) -- ^ An option for None if the field is optional
        -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> WidgetFor site ()) -- ^ Other options
        -> HandlerFor site (OptionList a)
        -> Field (HandlerFor site) a
selectFieldHelper outside onOpt inside opts' = Field
    { fieldParse = \x _ -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name attrs val isReq -> do
        opts <- fmap olOptions $ handlerToWidget opts'
        outside theId name attrs $ do
            unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                ((if isReq then (("required", "required"):) else id) attrs)
                (optionExternalValue opt)
                ((render opts val) == optionExternalValue opt)
                (optionDisplay opt)
    , fieldEnctype = UrlEncoded
    }
  where
    render _ (Left _) = ""
    render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter ((== a) . optionInternalValue) opts
    selectParser _ [] = Right Nothing
    selectParser opts (s:_) = case s of
            "" -> Right Nothing
            "none" -> Right Nothing
            x -> case olReadExternal opts x of
                    Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
                    Just y -> Right $ Just y

-- | Creates an input with @type="file"@.
fileField :: Monad m
          => Field m FileInfo
fileField = Field
    { fieldParse = \_ files -> return $
        case files of
            [] -> Right Nothing
            file:_ -> Right $ Just file
    , fieldView = \id' name attrs _ isReq -> toWidget [hamlet|
            <input id=#{id'} name=#{name} *{attrs} type=file :isReq:required>
        |]
    , fieldEnctype = Multipart
    }

fileAFormReq :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
             => FieldSettings (HandlerSite m) -> AForm m FileInfo
fileAFormReq fs = AForm $ \(site, langs) menvs ints -> do
    let (name, ints') =
            case fsName fs of
                Just x -> (x, ints)
                Nothing ->
                    let i' = incrInts ints
                     in (pack $ 'f' : show i', i')
    id' <- maybe newIdent return $ fsId fs
    let (res, errs) =
            case menvs of
                Nothing -> (FormMissing, Nothing)
                Just (_, fenv) ->
                    case Map.lookup name fenv of
                        Just (fi:_) -> (FormSuccess fi, Nothing)
                        _ ->
                            let t = renderMessage site langs MsgValueRequired
                             in (FormFailure [t], Just $ toHtml t)
    let fv = FieldView
            { fvLabel = toHtml $ renderMessage site langs $ fsLabel fs
            , fvTooltip = fmap (toHtml . renderMessage site langs) $ fsTooltip fs
            , fvId = id'
            , fvInput = [whamlet|
$newline never
<input type=file name=#{name} ##{id'} *{fsAttrs fs}>
|]
            , fvErrors = errs
            , fvRequired = True
            }
    return (res, (fv :), ints', Multipart)

fileAFormOpt :: MonadHandler m
             => FieldSettings (HandlerSite m)
             -> AForm m (Maybe FileInfo)
fileAFormOpt fs = AForm $ \(master, langs) menvs ints -> do
    let (name, ints') =
            case fsName fs of
                Just x -> (x, ints)
                Nothing ->
                    let i' = incrInts ints
                     in (pack $ 'f' : show i', i')
    id' <- maybe newIdent return $ fsId fs
    let (res, errs) =
            case menvs of
                Nothing -> (FormMissing, Nothing)
                Just (_, fenv) ->
                    case Map.lookup name fenv of
                        Just (fi:_) -> (FormSuccess $ Just fi, Nothing)
                        _ -> (FormSuccess Nothing, Nothing)
    let fv = FieldView
            { fvLabel = toHtml $ renderMessage master langs $ fsLabel fs
            , fvTooltip = fmap (toHtml . renderMessage master langs) $ fsTooltip fs
            , fvId = id'
            , fvInput = [whamlet|
$newline never
<input type=file name=#{name} ##{id'} *{fsAttrs fs}>
|]
            , fvErrors = errs
            , fvRequired = False
            }
    return (res, (fv :), ints', Multipart)

incrInts :: Ints -> Ints
incrInts (IntSingle i) = IntSingle $ i + 1
incrInts (IntCons i is) = (i + 1) `IntCons` is


-- | Adds a '0' to some text so that it may be recognized as a double.
--   The read ftn does not recognize ".3" as 0.3 nor "-.3" as -0.3, so this
--   function changes ".xxx" to "0.xxx" and "-.xxx" to "-0.xxx"

prependZero :: Text -> Text
prependZero t0 = if T.null t1
                 then t1
                 else if T.head t1 == '.'
                      then '0' `T.cons` t1
                      else if "-." `T.isPrefixOf` t1
                           then "-0." `T.append` (T.drop 2 t1)
                           else t1

  where t1 = T.dropWhile ((==) ' ') t0

-- $optionsOverview
-- These functions create inputs where one or more options can be selected from a list.
-- 
-- The basic datastructure used is an 'Option', which combines a user-facing display value, the internal Haskell value being selected, and an external 'Text' stored as the @value@ in the form (used to map back to the internal value). A list of these, together with a function mapping from an external value back to a Haskell value, form an 'OptionList', which several of these functions take as an argument.
-- 
-- Typically, you won't need to create an 'OptionList' directly and can instead make one with functions like 'optionsPairs' or 'optionsEnum'. Alternatively, you can use functions like 'selectFieldList', which use their @[(msg, a)]@ parameter to create an 'OptionList' themselves.
