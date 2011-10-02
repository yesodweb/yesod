{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , htmlField
    , emailField
    , searchField
    , selectField
    , multiSelectField
    , AutoFocus
    , urlField
    , doubleField
    , parseDate
    , parseTime
    , Textarea (..)
    , radioField
    , boolField
      -- * File 'AForm's
    , fileAFormReq
    , fileAFormOpt
      -- * Options
    , selectField'
    , radioField'
    , Option (..)
    , OptionList (..)
    , mkOptionList
    , optionsPersist
    , optionsPairs
    , optionsEnum
    ) where

import Yesod.Form.Types
import Yesod.Form.I18n.English
import Yesod.Widget
import Yesod.Message (RenderMessage (renderMessage), SomeMessage (..))
import Text.Hamlet
import Text.Blaze (ToHtml (..), preEscapedText, unsafeByteString)
import Text.Cassius
import Data.Time (Day, TimeOfDay(..))
import qualified Text.Email.Validate as Email
import Network.URI (parseURI)
import Database.Persist (PersistField)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Control.Monad (when, unless)
import Data.List (intersect, nub)
import Data.Either (rights)
import Data.Maybe (catMaybes, listToMaybe)

import qualified Blaze.ByteString.Builder.Html.Utf8 as B
import Blaze.ByteString.Builder (writeByteString, toLazyByteString)
import Blaze.ByteString.Builder.Internal.Write (fromWriteList)

import Text.Blaze.Renderer.String (renderHtml)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Read
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import Control.Applicative ((<$>))
import qualified Data.Map as Map
import Yesod.Handler (newIdent, liftIOHandler)
import Yesod.Request (FileInfo)

import Yesod.Core (toSinglePiece, GGHandler, SinglePiece)
import Yesod.Persist (selectList, runDB, Filter, SelectOpt, YesodPersistBackend, Key, YesodPersist, PersistEntity, PersistBackend)
import Control.Arrow ((&&&))

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


defaultFormMessage :: FormMessage -> Text
defaultFormMessage = englishFormMessage

blank :: (Monad m, RenderMessage master FormMessage)
      => (Text -> Either FormMessage a) -> [Text] -> m (Either (SomeMessage master) (Maybe a))
blank _ [] = return $ Right Nothing
blank _ ("":_) = return $ Right Nothing
blank f (x:_) = return $ either (Left . SomeMessage) (Right . Just) $ f x

intField :: (Integral i, RenderMessage master FormMessage) => Field sub master i
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
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)

doubleField :: RenderMessage master FormMessage => Field sub master Double
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
  where showVal = either id (pack . show)

dayField :: RenderMessage master FormMessage => Field sub master Day
dayField = Field
    { fieldParse = blank $ parseDate . unpack
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="date" :isReq:required="" value="#{showVal val}">
|]
    }
  where showVal = either id (pack . show)

timeField :: RenderMessage master FormMessage => Field sub master TimeOfDay
timeField = Field
    { fieldParse = blank $ parseTime . unpack
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" :isReq:required="" value="#{showVal val}">
|]
    }
  where
    showVal = either id (pack . show . roundFullSeconds)
    roundFullSeconds tod =
        TimeOfDay (todHour tod) (todMin tod) fullSec
      where
        fullSec = fromInteger $ floor $ todSec tod

htmlField :: RenderMessage master FormMessage => Field sub master Html
htmlField = Field
    { fieldParse = blank $ Right . preEscapedText . sanitizeBalance
    , fieldView = \theId name val _isReq -> addHamlet
        [HAMLET|\
<textarea id="#{theId}" name="#{name}" .html>#{showVal val}
|]
    }
  where showVal = either id (pack . renderHtml)

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

textareaField :: RenderMessage master FormMessage => Field sub master Textarea
textareaField = Field
    { fieldParse =  blank $ Right . Textarea
    , fieldView = \theId name val _isReq -> addHamlet
        [HAMLET|\
<textarea id="#{theId}" name="#{name}">#{either id unTextarea val}
|]
    }

hiddenField :: RenderMessage master FormMessage => Field sub master Text
hiddenField = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name val _isReq -> addHamlet
        [HAMLET|\
<input type="hidden" id="#{theId}" name="#{name}" value="#{either id id val}">
|]
    }

textField :: RenderMessage master FormMessage => Field sub master Text
textField = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name val isReq ->
        [WHAMLET|
<input id="#{theId}" name="#{name}" type="text" :isReq:required value="#{either id id val}">
|]
    }

passwordField :: RenderMessage master FormMessage => Field sub master Text
passwordField = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="password" :isReq:required="" value="#{either id id val}">
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

emailField :: RenderMessage master FormMessage => Field sub master Text
emailField = Field
    { fieldParse = blank $
        \s -> if Email.isValid (unpack s)
                then Right s
                else Left $ MsgInvalidEmail s
    , fieldView = \theId name val isReq -> addHamlet
        [HAMLET|\
<input id="#{theId}" name="#{name}" type="email" :isReq:required="" value="#{either id id val}">
|]
    }

type AutoFocus = Bool
searchField :: RenderMessage master FormMessage => AutoFocus -> Field sub master Text
searchField autoFocus = Field
    { fieldParse = blank Right
    , fieldView = \theId name val isReq -> do
        [WHAMLET|\
<input id="#{theId}" name="#{name}" type="search" :isReq:required="" :autoFocus:autofocus="" value="#{either id id val}">
|]
        when autoFocus $ do
          -- we want this javascript to be placed immediately after the field
          [WHAMLET|\<script>if (!('autofocus' in document.createElement('input'))) {document.getElementById('#{theId}').focus();}</script> 
|]
          addCassius [CASSIUS|
            #{theId}
              -webkit-appearance: textfield
            |]
    }

urlField :: RenderMessage master FormMessage => Field sub master Text
urlField = Field
    { fieldParse = blank $ \s ->
        case parseURI $ unpack s of
            Nothing -> Left $ MsgInvalidUrl s
            Just _ -> Right s
    , fieldView = \theId name val isReq ->
        [WHAMLET|
<input ##{theId} name=#{name} type=url :isReq:required value=#{either id id val}>
|]
    }

selectField :: (Eq a, RenderMessage master FormMessage) => [(Text, a)] -> Field sub master a
selectField = selectField' . optionsPairs

selectField' :: (Eq a, RenderMessage master FormMessage) => GGHandler sub master IO (OptionList a) -> Field sub master a
selectField' = selectFieldHelper
    (\theId name inside -> [WHAMLET|<select ##{theId} name=#{name}>^{inside}|]) -- outside
    (\_theId _name isSel -> [WHAMLET|<option value=none :isSel:selected>_{MsgSelectNone}|]) -- onOpt
    (\_theId _name value isSel text -> [WHAMLET|<option value=#{value} :isSel:selected>#{text}|]) -- inside

multiSelectField :: (Show a, Eq a, RenderMessage master FormMessage) => [(Text, a)] -> Field sub master [a]
multiSelectField = multiSelectFieldHelper
    (\theId name inside -> [WHAMLET|<select ##{theId} multiple name=#{name}>^{inside}|])
    (\_theId _name value isSel text -> [WHAMLET|<option value=#{value} :isSel:selected>#{text}|])

radioField :: (Eq a, RenderMessage master FormMessage) => [(Text, a)] -> Field sub master a
radioField = radioField' . optionsPairs

radioField' :: (Eq a, RenderMessage master FormMessage) => GGHandler sub master IO (OptionList a) -> Field sub master a
radioField' = selectFieldHelper
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

boolField :: RenderMessage master FormMessage => Field sub master Bool
boolField = Field
      { fieldParse = return . boolParser
      , fieldView = \theId name val isReq -> [WHAMLET|
  $if not isReq
      <input id=#{theId}-none type=radio name=#{name} value=none checked>
      <label for=#{theId}-none>_{MsgSelectNone}


<input id=#{theId}-yes type=radio name=#{name} value=yes :showVal id val:checked>
<label for=#{theId}-yes>_{MsgBoolYes}

<input id=#{theId}-no type=radio name=#{name} value=no :showVal not val:checked>
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
      t -> Left $ SomeMessage $ MsgInvalidBool t
    showVal = either (\_ -> False)

multiSelectFieldHelper :: (Show a, Eq a)
        => (Text -> Text -> GWidget sub master () -> GWidget sub master ())
        -> (Text -> Text -> Text -> Bool -> Text -> GWidget sub master ())
        -> [(Text, a)] -> Field sub master [a]
multiSelectFieldHelper outside inside opts = Field
    { fieldParse = return . selectParser
    , fieldView = \theId name vals _ ->
        outside theId name $ do
            flip mapM_ pairs $ \pair -> inside
                theId
                name
                (pack $ show $ fst pair)
                ((fst pair) `elem` (either (\_ -> []) selectedVals vals)) -- We are presuming that select fields can't hold invalid values
                (fst $ snd pair)
    }
  where
    pairs = zip [1 :: Int ..] opts -- FIXME use IntMap
    rpairs = zip (map snd opts) [1 :: Int ..]
    selectedVals vals = map snd $ filter (\y -> fst y `elem` vals) rpairs
    selectParser [] = Right Nothing
    selectParser xs | not $ null (["", "none"] `intersect` xs) = Right Nothing
                    | otherwise = (Right . Just . map snd . catMaybes . map (\y -> lookup y pairs) . nub . map fst . rights . map Data.Text.Read.decimal) xs

data OptionList a = OptionList
    { olOptions :: [Option a]
    , olReadExternal :: Text -> Maybe a
    }

mkOptionList :: [Option a] -> OptionList a
mkOptionList os = OptionList
    { olOptions = os
    , olReadExternal = flip Map.lookup $ Map.fromList $ map (optionExternalValue &&& optionInternalValue) os
    }

data Option a = Option
    { optionDisplay :: Text
    , optionInternalValue :: a
    , optionExternalValue :: Text
    }

optionsPairs :: [(Text, a)] -> GGHandler sub master IO (OptionList a)
optionsPairs = return . mkOptionList . zipWith (\external (display, internal) -> Option
    { optionDisplay = display
    , optionInternalValue = internal
    , optionExternalValue = pack $ show external
    }) [1 :: Int ..]

optionsEnum :: (Show a, Enum a, Bounded a) => GGHandler sub master IO (OptionList a)
optionsEnum = optionsPairs $ map (\x -> (pack $ show x, x)) [minBound..maxBound]

optionsPersist :: ( YesodPersist master, PersistEntity a, PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)
                  , SinglePiece (Key (YesodPersistBackend master) a)
                  )
               => [Filter a] -> [SelectOpt a] -> (a -> Text) -> GGHandler sub master IO (OptionList (Key (YesodPersistBackend master) a, a))
optionsPersist filts ords toDisplay = fmap mkOptionList $ do
    pairs <- runDB $ selectList filts ords
    return $ map (\(key, value) -> Option
        { optionDisplay = toDisplay value
        , optionInternalValue = (key, value)
        , optionExternalValue = toSinglePiece key
        }) pairs

selectFieldHelper
        :: (Eq a, RenderMessage master FormMessage)
        => (Text -> Text -> GWidget sub master () -> GWidget sub master ())
        -> (Text -> Text -> Bool -> GWidget sub master ())
        -> (Text -> Text -> Text -> Bool -> Text -> GWidget sub master ())
        -> GGHandler sub master IO (OptionList a) -> Field sub master a
selectFieldHelper outside onOpt inside opts' = Field
    { fieldParse = \x -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name val isReq -> do
        opts <- fmap olOptions $ lift $ liftIOHandler opts'
        outside theId name $ do
            unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                (optionExternalValue opt)
                ((render opts val) == optionExternalValue opt)
                (optionDisplay opt)
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

fileAFormReq :: (RenderMessage master msg, RenderMessage master FormMessage) => FieldSettings msg -> AForm sub master FileInfo
fileAFormReq fs = AForm $ \(master, langs) menvs ints -> do
    let (name, ints') =
            case fsName fs of
                Just x -> (x, ints)
                Nothing ->
                    let i' = incrInts ints
                     in (pack $ 'f' : show i', i')
    id' <- maybe (pack <$> newIdent) return $ fsId fs
    let (res, errs) =
            case menvs of
                Nothing -> (FormMissing, Nothing)
                Just (_, fenv) ->
                    case Map.lookup name fenv of
                        Nothing ->
                            let t = renderMessage master langs MsgValueRequired
                             in (FormFailure [t], Just $ toHtml t)
                        Just fi -> (FormSuccess fi, Nothing)
    let fv = FieldView
            { fvLabel = toHtml $ renderMessage master langs $ fsLabel fs
            , fvTooltip = fmap (toHtml . renderMessage master langs) $ fsTooltip fs
            , fvId = id'
            , fvInput = [WHAMLET|
<input type=file name=#{name} ##{id'}>
|]
            , fvErrors = errs
            , fvRequired = True
            }
    return (res, (fv :), ints', Multipart)

fileAFormOpt :: (RenderMessage master msg, RenderMessage master FormMessage) => FieldSettings msg -> AForm sub master (Maybe FileInfo)
fileAFormOpt fs = AForm $ \(master, langs) menvs ints -> do
    let (name, ints') =
            case fsName fs of
                Just x -> (x, ints)
                Nothing ->
                    let i' = incrInts ints
                     in (pack $ 'f' : show i', i')
    id' <- maybe (pack <$> newIdent) return $ fsId fs
    let (res, errs) =
            case menvs of
                Nothing -> (FormMissing, Nothing)
                Just (_, fenv) ->
                    case Map.lookup name fenv of
                        Nothing -> (FormSuccess Nothing, Nothing)
                        Just fi -> (FormSuccess $ Just fi, Nothing)
    let fv = FieldView
            { fvLabel = toHtml $ renderMessage master langs $ fsLabel fs
            , fvTooltip = fmap (toHtml . renderMessage master langs) $ fsTooltip fs
            , fvId = id'
            , fvInput = [WHAMLET|
<input type=file name=#{name} ##{id'}>
|]
            , fvErrors = errs
            , fvRequired = False
            }
    return (res, (fv :), ints', Multipart)

incrInts :: Ints -> Ints
incrInts (IntSingle i) = IntSingle $ i + 1
incrInts (IntCons i is) = (i + 1) `IntCons` is
