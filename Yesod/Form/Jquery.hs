{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | Some fields spiced up with jQuery UI.
module Yesod.Form.Jquery
    ( YesodJquery (..)
    , jqueryDayField
    , maybeJqueryDayField
    , jqueryDayTimeField
    , jqueryDayTimeFieldProfile
    , jqueryAutocompleteField
    , maybeJqueryAutocompleteField
    , jqueryDayFieldProfile
    , googleHostedJqueryUiCss
    , JqueryDaySettings (..)
    , Default (..)
    ) where

import Yesod.Handler
import Yesod.Form.Core
import Yesod.Form.Profiles
import Yesod.Widget
import Data.Time (UTCTime (..), Day, TimeOfDay (..), timeOfDayToTime,
                  timeToTimeOfDay)
import Yesod.Hamlet
import Data.Char (isSpace)
import Data.Default

-- | Gets the Google hosted jQuery UI 1.8 CSS file with the given theme.
googleHostedJqueryUiCss :: String -> String
googleHostedJqueryUiCss theme = concat
    [ "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/"
    , theme
    , "/jquery-ui.css"
    ]

class YesodJquery a where
    -- | The jQuery 1.4 Javascript file.
    urlJqueryJs :: a -> Either (Route a) String
    urlJqueryJs _ = Right "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"

    -- | The jQuery UI 1.8 Javascript file.
    urlJqueryUiJs :: a -> Either (Route a) String
    urlJqueryUiJs _ = Right "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"

    -- | The jQuery UI 1.8 CSS file; defaults to cupertino theme.
    urlJqueryUiCss :: a -> Either (Route a) String
    urlJqueryUiCss _ = Right $ googleHostedJqueryUiCss "cupertino"

    -- | jQuery UI time picker add-on.
    urlJqueryUiDateTimePicker :: a -> Either (Route a) String
    urlJqueryUiDateTimePicker _ = Right "http://github.com/gregwebs/jquery.ui.datetimepicker/raw/master/jquery.ui.datetimepicker.js"

jqueryDayField :: (IsForm f, FormType f ~ Day, YesodJquery (FormMaster f))
               => JqueryDaySettings
               -> FormFieldSettings
               -> Maybe (FormType f)
               -> f
jqueryDayField = requiredFieldHelper . jqueryDayFieldProfile

maybeJqueryDayField
    :: (IsForm f, FormType f ~ Maybe Day, YesodJquery (FormMaster f))
    => JqueryDaySettings
    -> FormFieldSettings
    -> Maybe (FormType f)
    -> f
maybeJqueryDayField = optionalFieldHelper . jqueryDayFieldProfile

jqueryDayFieldProfile :: YesodJquery y
                      => JqueryDaySettings -> FieldProfile sub y Day
jqueryDayFieldProfile jds = FieldProfile
    { fpParse = maybe
                  (Left "Invalid day, must be in YYYY-MM-DD format")
                  Right
              . readMay
    , fpRender = show
    , fpWidget = \theId name val isReq -> do
        addHtml
#if GHC7
                [hamlet|
#else
                [$hamlet|
#endif
%input#$theId$!name=$name$!type=date!:isReq:required!value=$val$
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        addJulius
#if GHC7
                [julius|
#else
                [$julius|
#endif
$(function(){$("#%theId%").datepicker({
    dateFormat:'yy-mm-dd',
    changeMonth:%jsBool.jdsChangeMonth.jds%,
    changeYear:%jsBool.jdsChangeYear.jds%,
    numberOfMonths:%mos.jdsNumberOfMonths.jds%,
    yearRange:"%jdsYearRange.jds%"
})});
|]
    }
  where
    jsBool True = "true"
    jsBool False = "false"
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

jqueryDayTimeField
    :: (IsForm f, FormType f ~ UTCTime, YesodJquery (FormMaster f))
    => FormFieldSettings
    -> Maybe (FormType f)
    -> f
jqueryDayTimeField = requiredFieldHelper jqueryDayTimeFieldProfile

-- use A.M/P.M and drop seconds and "UTC" (as opposed to normal UTCTime show)
jqueryDayTimeUTCTime :: UTCTime -> String
jqueryDayTimeUTCTime (UTCTime day utcTime) =
  let timeOfDay = timeToTimeOfDay utcTime
  in (replace '-' '/' (show day)) ++ " " ++ showTimeOfDay timeOfDay
  where
    showTimeOfDay (TimeOfDay hour minute _) =
      let (h, apm) = if hour < 12 then (hour, "AM") else (hour - 12, "PM")
      in (show h) ++ ":" ++ (showLeadingZero minute) ++ " " ++ apm

jqueryDayTimeFieldProfile :: YesodJquery y => FieldProfile sub y UTCTime
jqueryDayTimeFieldProfile = FieldProfile
    { fpParse  = parseUTCTime
    , fpRender = jqueryDayTimeUTCTime
    , fpWidget = \theId name val isReq -> do
        addHtml
#if GHC7
                [hamlet|
#else
                [$hamlet|
#endif
%input#$theId$!name=$name$!:isReq:required!value=$val$
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addScript' urlJqueryUiDateTimePicker
        addStylesheet' urlJqueryUiCss
        addJulius
#if GHC7
                [julius|
#else
                [$julius|
#endif
$(function(){$("#%theId%").datetimepicker({dateFormat : "yyyy/mm/dd h:MM TT"})});
|]
    }

parseUTCTime :: String -> Either String UTCTime
parseUTCTime s =
    let (dateS, timeS) = break isSpace (dropWhile isSpace s)
        dateE = parseDate dateS
     in case dateE of
            Left l -> Left l
            Right date ->
                ifRight (parseTime timeS)
                    (UTCTime date . timeOfDayToTime)

jqueryAutocompleteField
    :: (IsForm f, FormType f ~ String, YesodJquery (FormMaster f))
    => Route (FormMaster f)
    -> FormFieldSettings
    -> Maybe (FormType f)
    -> f
jqueryAutocompleteField = requiredFieldHelper . jqueryAutocompleteFieldProfile

maybeJqueryAutocompleteField
    :: (IsForm f, FormType f ~ Maybe String, YesodJquery (FormMaster f))
    => Route (FormMaster f)
    -> FormFieldSettings
    -> Maybe (FormType f)
    -> f
maybeJqueryAutocompleteField src =
    optionalFieldHelper $ jqueryAutocompleteFieldProfile src

jqueryAutocompleteFieldProfile :: YesodJquery y => Route y -> FieldProfile sub y String
jqueryAutocompleteFieldProfile src = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpWidget = \theId name val isReq -> do
        addHtml
#if GHC7
                [hamlet|
#else
                [$hamlet|
#endif
%input.autocomplete#$theId$!name=$name$!type=text!:isReq:required!value=$val$
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        addJulius
#if GHC7
                [julius|
#else
                [$julius|
#endif
$(function(){$("#%theId%").autocomplete({source:"@src@",minLength:2})});
|]
    }

addScript' :: (y -> Either (Route y) String) -> GWidget sub y ()
addScript' f = do
    y <- liftHandler getYesod
    addScriptEither $ f y

addStylesheet' :: (y -> Either (Route y) String) -> GWidget sub y ()
addStylesheet' f = do
    y <- liftHandler getYesod
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
