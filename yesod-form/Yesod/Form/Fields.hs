{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , AutoFocus
    , urlField
    , doubleField
    , parseDate
    , parseTime
    , Textarea (..)
    , boolField
    , checkBoxField
      -- * File 'AForm's
    , fileAFormReq
    , fileAFormOpt
      -- * Options
    , selectField
    , selectFieldList
    , radioField
    , radioFieldList
    , multiSelectField
    , multiSelectFieldList
    , Option (..)
    , OptionList (..)
    , mkOptionList
    , optionsPersist
    , optionsPairs
    , optionsEnum
    ) where

import Yesod.Form.Types
import Yesod.Form.I18n.English
import Yesod.Handler (getMessageRender)
import Yesod.Widget (toWidget, whamlet, GWidget)
import Yesod.Message (RenderMessage (renderMessage), SomeMessage (..))
import Text.Hamlet
import Text.Blaze (ToHtml (..), preEscapedText, unsafeByteString)
import Text.Cassius
import Data.Time (Day, TimeOfDay(..))
import qualified Text.Email.Validate as Email
import Network.URI (parseURI)
import Database.Persist (PersistField)
import Database.Persist.Store (Entity (..))
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Control.Monad (when, unless)
import Data.Maybe (listToMaybe)

import qualified Blaze.ByteString.Builder.Html.Utf8 as B
import Blaze.ByteString.Builder (writeByteString, toLazyByteString)
import Blaze.ByteString.Builder.Internal.Write (fromWriteList)
import Database.Persist.Store (PersistEntityBackend)

import Text.Blaze.Renderer.String (renderHtml)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Read

import qualified Data.Map as Map
import Yesod.Handler (newIdent, lift)
import Yesod.Request (FileInfo)

import Yesod.Core (toPathPiece, GHandler, PathPiece, fromPathPiece)
import Yesod.Persist (selectList, runDB, Filter, SelectOpt, YesodPersistBackend, Key, YesodPersist, PersistEntity, PersistQuery)
import Control.Arrow ((&&&))

import Control.Applicative ((<$>))

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

    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="number" :isReq:required="" value="#{showVal val}">
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

    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required="" value="#{showVal val}">
|]
    }
  where showVal = either id (pack . show)

dayField :: RenderMessage master FormMessage => Field sub master Day
dayField = Field
    { fieldParse = blank $ parseDate . unpack
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="date" :isReq:required="" value="#{showVal val}">
|]
    }
  where showVal = either id (pack . show)

timeField :: RenderMessage master FormMessage => Field sub master TimeOfDay
timeField = Field
    { fieldParse = blank $ parseTime . unpack
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} :isReq:required="" value="#{showVal val}">
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
    , fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
$# FIXME: There was a class="html" attribute, for what purpose?
<textarea id="#{theId}" name="#{name}" *{attrs}>#{showVal val}
|]
    }
  where showVal = either id (pack . renderHtml)

-- | A newtype wrapper around a 'Text' that converts newlines to HTML
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
    , fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unTextarea val}
|]
    }

hiddenField :: (PathPiece p, RenderMessage master FormMessage)
            => Field sub master p
hiddenField = Field
    { fieldParse = blank $ maybe (Left MsgValueRequired) Right . fromPathPiece
    , fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
<input type="hidden" id="#{theId}" name="#{name}" *{attrs} value="#{either id toPathPiece val}">
|]
    }

textField :: RenderMessage master FormMessage => Field sub master Text
textField = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either id id val}">
|]
    }

passwordField :: RenderMessage master FormMessage => Field sub master Text
passwordField = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="password" :isReq:required="" value="#{either id id val}">
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
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="email" :isReq:required="" value="#{either id id val}">
|]
    }

type AutoFocus = Bool
searchField :: RenderMessage master FormMessage => AutoFocus -> Field sub master Text
searchField autoFocus = Field
    { fieldParse = blank Right
    , fieldView = \theId name attrs val isReq -> do
        [whamlet|\
<input id="#{theId}" name="#{name}" *{attrs} type="search" :isReq:required="" :autoFocus:autofocus="" value="#{either id id val}">
|]
        when autoFocus $ do
          -- we want this javascript to be placed immediately after the field
          [whamlet|<script>if (!('autofocus' in document.createElement('input'))) {document.getElementById('#{theId}').focus();}|]
          toWidget [cassius|
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
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
<input ##{theId} name=#{name} *{attrs} type=url :isReq:required value=#{either id id val}>
|]
    }

selectFieldList :: (Eq a, RenderMessage master FormMessage, RenderMessage master msg) => [(msg, a)] -> Field sub master a
selectFieldList = selectField . optionsPairs

selectField :: (Eq a, RenderMessage master FormMessage) => GHandler sub master (OptionList a) -> Field sub master a
selectField = selectFieldHelper
    (\theId name inside -> [whamlet|<select ##{theId} name=#{name}>^{inside}|]) -- outside
    (\_theId _name isSel -> [whamlet|<option value=none :isSel:selected>_{MsgSelectNone}|]) -- onOpt
    (\_theId _name attrs value isSel text -> [whamlet|<option value=#{value} :isSel:selected *{attrs}>#{text}|]) -- inside

multiSelectFieldList :: (Eq a, RenderMessage master FormMessage, RenderMessage master msg) => [(msg, a)] -> Field sub master [a]
multiSelectFieldList = multiSelectField . optionsPairs

multiSelectField :: (Eq a, RenderMessage master FormMessage)
                 => GHandler sub master (OptionList a)
                 -> Field sub master [a]
multiSelectField ioptlist =
    Field parse view
  where
    parse [] = return $ Right Nothing
    parse optlist = do
        mapopt <- olReadExternal <$> ioptlist
        case mapM mapopt optlist of
             Nothing -> return $ Left "Error parsing values"
             Just res -> return $ Right $ Just res

    view theId name attrs val isReq = do
        opts <- fmap olOptions $ lift ioptlist
        let selOpts = map (id &&& (optselected val)) opts
        [whamlet|
            <select ##{theId} name=#{name} :isReq:required multiple *{attrs}>
                $forall (opt, optsel) <- selOpts
                    <option value=#{optionExternalValue opt} :optsel:selected>#{optionDisplay opt}
                |]
        where
            optselected (Left _) _ = False
            optselected (Right vals) opt = (optionInternalValue opt) `elem` vals

radioFieldList :: (Eq a, RenderMessage master FormMessage, RenderMessage master msg) => [(msg, a)] -> Field sub master a
radioFieldList = radioField . optionsPairs

radioField :: (Eq a, RenderMessage master FormMessage) => GHandler sub master (OptionList a) -> Field sub master a
radioField = selectFieldHelper
    (\theId _name inside -> [whamlet|<div ##{theId}>^{inside}|])
    (\theId name isSel -> [whamlet|
<div>
    <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
    <label for=#{theId}-none>_{MsgSelectNone}
|])
    (\theId name attrs value isSel text -> [whamlet|
<div>
    <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
    <label for=#{theId}-#{value}>#{text}
|])

boolField :: RenderMessage master FormMessage => Field sub master Bool
boolField = Field
      { fieldParse = return . boolParser
      , fieldView = \theId name attrs val isReq -> [whamlet|
  $if not isReq
      <input id=#{theId}-none *{attrs} type=radio name=#{name} value=none checked>
      <label for=#{theId}-none>_{MsgSelectNone}


<input id=#{theId}-yes *{attrs} type=radio name=#{name} value=yes :showVal id val:checked>
<label for=#{theId}-yes>_{MsgBoolYes}

<input id=#{theId}-no *{attrs} type=radio name=#{name} value=no :showVal not val:checked>
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

-- | While the default @'boolField'@ implements a radio button so you
--   can differentiate between an empty response (Nothing) and a no
--   response (Just False), this simpler checkbox field returns an empty
--   response as Just False.
--
--   Note that this makes the field always optional.
--
checkBoxField :: RenderMessage m FormMessage => Field s m Bool
checkBoxField = Field
    { fieldParse = return . checkBoxParser
    , fieldView  = \theId name attrs val _ -> [whamlet|
<input id=#{theId} *{attrs} type=checkbox name=#{name} value=yes :showVal id val:checked>
|]
    }

    where
        checkBoxParser [] = Right $ Just False
        checkBoxParser (x:_) = case x of
            "yes" -> Right $ Just True
            _     -> Right $ Just False

        showVal = either (\_ -> False)

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

optionsPairs :: RenderMessage master msg => [(msg, a)] -> GHandler sub master (OptionList a)
optionsPairs opts = do
  mr <- getMessageRender
  let mkOption external (display, internal) =
          Option { optionDisplay       = mr display
                 , optionInternalValue = internal
                 , optionExternalValue = pack $ show external
                 }
  return $ mkOptionList (zipWith mkOption [1 :: Int ..] opts)

optionsEnum :: (Show a, Enum a, Bounded a) => GHandler sub master (OptionList a)
optionsEnum = optionsPairs $ map (\x -> (pack $ show x, x)) [minBound..maxBound]

optionsPersist :: ( YesodPersist master, PersistEntity a
                  , PersistQuery (YesodPersistBackend master) (GHandler sub master)
                  , PathPiece (Key (YesodPersistBackend master) a)
                  , RenderMessage master msg
                  , PersistEntityBackend a ~ YesodPersistBackend master
                  )
               => [Filter a] -> [SelectOpt a] -> (a -> msg) -> GHandler sub master (OptionList (Entity a))
optionsPersist filts ords toDisplay = fmap mkOptionList $ do
    mr <- getMessageRender
    pairs <- runDB $ selectList filts ords
    return $ map (\(Entity key value) -> Option
        { optionDisplay = mr (toDisplay value)
        , optionInternalValue = Entity key value
        , optionExternalValue = toPathPiece key
        }) pairs

selectFieldHelper
        :: (Eq a, RenderMessage master FormMessage)
        => (Text -> Text -> GWidget sub master () -> GWidget sub master ())
        -> (Text -> Text -> Bool -> GWidget sub master ())
        -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> GWidget sub master ())
        -> GHandler sub master (OptionList a) -> Field sub master a
selectFieldHelper outside onOpt inside opts' = Field
    { fieldParse = \x -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name attrs val isReq -> do
        opts <- fmap olOptions $ lift opts'
        outside theId name $ do
            unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                attrs
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

fileAFormReq :: RenderMessage master FormMessage => FieldSettings master -> AForm sub master FileInfo
fileAFormReq fs = AForm $ \(master, langs) menvs ints -> do
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
                        Nothing ->
                            let t = renderMessage master langs MsgValueRequired
                             in (FormFailure [t], Just $ toHtml t)
                        Just fi -> (FormSuccess fi, Nothing)
    let fv = FieldView
            { fvLabel = toHtml $ renderMessage master langs $ fsLabel fs
            , fvTooltip = fmap (toHtml . renderMessage master langs) $ fsTooltip fs
            , fvId = id'
            , fvInput = [whamlet|
<input type=file name=#{name} ##{id'} *{fsAttrs fs}>
|]
            , fvErrors = errs
            , fvRequired = True
            }
    return (res, (fv :), ints', Multipart)

fileAFormOpt :: RenderMessage master FormMessage => FieldSettings master -> AForm sub master (Maybe FileInfo)
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
                        Nothing -> (FormSuccess Nothing, Nothing)
                        Just fi -> (FormSuccess $ Just fi, Nothing)
    let fv = FieldView
            { fvLabel = toHtml $ renderMessage master langs $ fsLabel fs
            , fvTooltip = fmap (toHtml . renderMessage master langs) $ fsTooltip fs
            , fvId = id'
            , fvInput = [whamlet|
<input type=file name=#{name} ##{id'} *{fsAttrs fs}>
|]
            , fvErrors = errs
            , fvRequired = False
            }
    return (res, (fv :), ints', Multipart)

incrInts :: Ints -> Ints
incrInts (IntSingle i) = IntSingle $ i + 1
incrInts (IntCons i is) = (i + 1) `IntCons` is
