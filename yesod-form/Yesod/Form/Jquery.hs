{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME remove
-- | Some fields spiced up with jQuery UI.
module Yesod.Form.Jquery
    ( YesodJquery (..)
    , jqueryDayField
    , jqueryDayTimeField
    , jqueryAutocompleteField
    , googleHostedJqueryUiCss
    , JqueryDaySettings (..)
    , Default (..)
    ) where

import Yesod.Handler
import Yesod.Form
import Yesod.Widget
import Data.Time (UTCTime (..), Day, TimeOfDay (..), timeOfDayToTime,
                  timeToTimeOfDay)
import Data.Char (isSpace)
import Data.Default
import Text.Hamlet (shamlet)
import Text.Julius (julius)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text, pack, unpack)
import Data.Monoid (mconcat)
import Yesod.Core (RenderMessage, SomeMessage (..))

#if __GLASGOW_HASKELL__ >= 700
#define HTML shamlet
#define HAMLET hamlet
#define CASSIUS cassius
#define JULIUS julius
#else
#define HTML $shamlet
#define HAMLET $hamlet
#define CASSIUS $cassius
#define JULIUS $julius
#endif

-- | Gets the Google hosted jQuery UI 1.8 CSS file with the given theme.
googleHostedJqueryUiCss :: Text -> Text
googleHostedJqueryUiCss theme = mconcat
    [ "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/"
    , theme
    , "/jquery-ui.css"
    ]

class YesodJquery a where
    -- | The jQuery 1.4 Javascript file.
    urlJqueryJs :: a -> Either (Route a) Text
    urlJqueryJs _ = Right "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"

    -- | The jQuery UI 1.8 Javascript file.
    urlJqueryUiJs :: a -> Either (Route a) Text
    urlJqueryUiJs _ = Right "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"

    -- | The jQuery UI 1.8 CSS file; defaults to cupertino theme.
    urlJqueryUiCss :: a -> Either (Route a) Text
    urlJqueryUiCss _ = Right $ googleHostedJqueryUiCss "cupertino"

    -- | jQuery UI time picker add-on.
    urlJqueryUiDateTimePicker :: a -> Either (Route a) Text
    urlJqueryUiDateTimePicker _ = Right "http://github.com/gregwebs/jquery.ui.datetimepicker/raw/master/jquery.ui.datetimepicker.js"

blank :: (RenderMessage master FormMessage, Monad m) => (Text -> Either FormMessage a) -> [Text] -> m (Either (SomeMessage master) (Maybe a))
blank _ [] = return $ Right Nothing
blank _ ("":_) = return $ Right Nothing
blank f (x:_) = return $ either (Left . SomeMessage) (Right . Just) $ f x

jqueryDayField :: (RenderMessage master FormMessage, YesodJquery master) => JqueryDaySettings -> Field sub master Day
jqueryDayField jds = Field
    { fieldParse = blank $ maybe
                  (Left MsgInvalidDay)
                  Right
              . readMay
              . unpack
    , fieldView = \theId name val isReq -> do
        addHtml [HTML|\
<input id="#{theId}" name="#{name}" type="date" :isReq:required="" value="#{showVal val}">
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        addJulius [JULIUS|
$(function(){
    var i = $("##{theId}");
    if (i.attr("type") != "date") {
        i.datepicker({
            dateFormat:'yy-mm-dd',
            changeMonth:#{jsBool $ jdsChangeMonth jds},
            changeYear:#{jsBool $ jdsChangeYear jds},
            numberOfMonths:#{mos $ jdsNumberOfMonths jds},
            yearRange:"#{jdsYearRange jds}"
        });
    }
});
|]
    }
  where
    showVal = either id (pack . show)
    jsBool True = "true" :: Text
    jsBool False = "false" :: Text
    mos (Left i) = show i
    mos (Right (x, y)) = concat
        [ "["
        , show x
        , ","
        , show y
        , "]"
        ]

ifRight :: Either a b -> (b -> c) -> Either a c
ifRight e f = case e of
            Left l  -> Left l
            Right r -> Right $ f r

showLeadingZero :: (Show a) => a -> String
showLeadingZero time = let t = show time in if length t == 1 then "0" ++ t else t

-- use A.M/P.M and drop seconds and "UTC" (as opposed to normal UTCTime show)
jqueryDayTimeUTCTime :: UTCTime -> String
jqueryDayTimeUTCTime (UTCTime day utcTime) =
  let timeOfDay = timeToTimeOfDay utcTime
  in (replace '-' '/' (show day)) ++ " " ++ showTimeOfDay timeOfDay
  where
    showTimeOfDay (TimeOfDay hour minute _) =
      let (h, apm) = if hour < 12 then (hour, "AM") else (hour - 12, "PM")
      in (showLeadingZero h) ++ ":" ++ (showLeadingZero minute) ++ " " ++ apm

jqueryDayTimeField :: (RenderMessage master FormMessage, YesodJquery master) => Field sub master UTCTime
jqueryDayTimeField = Field
    { fieldParse  = blank $ parseUTCTime . unpack
    , fieldView = \theId name val isReq -> do
        addHtml [HTML|\
<input id="#{theId}" name="#{name}" :isReq:required="" value="#{showVal val}">
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addScript' urlJqueryUiDateTimePicker
        addStylesheet' urlJqueryUiCss
        addJulius [JULIUS|
$(function(){$("##{theId}").datetimepicker({dateFormat : "yyyy/mm/dd hh:MM TT"})});
|]
    }
  where
   showVal  = either id (pack . jqueryDayTimeUTCTime)

parseUTCTime :: String -> Either FormMessage UTCTime
parseUTCTime s =
    let (dateS, timeS') = break isSpace (dropWhile isSpace s)
        timeS = drop 1 timeS'
        dateE = parseDate dateS
     in case dateE of
            Left l -> Left l
            Right date ->
                ifRight (parseTime timeS)
                    (UTCTime date . timeOfDayToTime)

jqueryAutocompleteField :: (RenderMessage master FormMessage, YesodJquery master)
                        => Route master -> Field sub master Text
jqueryAutocompleteField src = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name val isReq -> do
        addHtml [HTML|\
<input id="#{theId}" name="#{name}" type="text" :isReq:required="" value="#{either id id val}" .autocomplete>
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        addJulius [JULIUS|
$(function(){$("##{theId}").autocomplete({source:"@{src}",minLength:2})});
|]
    }

addScript' :: Monad m => (t -> Either (Route master) Text) -> GGWidget master (GGHandler sub t m) ()
addScript' f = do
    y <- lift getYesod
    addScriptEither $ f y

addStylesheet' :: (y -> Either (Route y) Text) -> GWidget sub y ()
addStylesheet' f = do
    y <- lift getYesod
    addStylesheetEither $ f y

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

-- | Replaces all instances of a value in a list by another value.
-- from http://hackage.haskell.org/packages/archive/cgi/3001.1.7.1/doc/html/src/Network-CGI-Protocol.html#replace
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

data JqueryDaySettings = JqueryDaySettings
    { jdsChangeMonth :: Bool
    , jdsChangeYear :: Bool
    , jdsYearRange :: String
    , jdsNumberOfMonths :: Either Int (Int, Int)
    }

instance Default JqueryDaySettings where
    def = JqueryDaySettings
        { jdsChangeMonth = False
        , jdsChangeYear = False
        , jdsYearRange = "c-10:c+10"
        , jdsNumberOfMonths = Left 1
        }
