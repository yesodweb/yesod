{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some fields spiced up with jQuery UI.
module Yesod.Form.Jquery
    ( YesodJquery (..)
    , jqueryDayField
    , jqueryAutocompleteField
    , googleHostedJqueryUiCss
    , JqueryDaySettings (..)
    , Default (..)
    ) where

import Yesod.Handler
import Yesod.Core (Route)
import Yesod.Form
import Yesod.Widget
import Data.Time (Day)
import Data.Default
import Text.Hamlet (shamlet)
import Text.Julius (julius, rawJS)
import Data.Text (Text, pack, unpack)
import Data.Monoid (mconcat)
import Yesod.Core (RenderMessage)
import Data.Aeson (toJSON)

-- | Gets the Google hosted jQuery UI 1.8 CSS file with the given theme.
googleHostedJqueryUiCss :: Text -> Text
googleHostedJqueryUiCss theme = mconcat
    [ "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/"
    , theme
    , "/jquery-ui.css"
    ]

class YesodJquery a where
    -- | The jQuery Javascript file. Note that in upgrades to this library, the
    -- version of jQuery referenced, or where it is downloaded from, may be
    -- changed without warning. If you are relying on a specific version of
    -- jQuery, you should give an explicit URL instead of relying on the
    -- default value.
    --
    -- Currently, the default value is jQuery 1.7 from Google\'s CDN.
    urlJqueryJs :: a -> Either (Route a) Text
    urlJqueryJs _ = Right "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"

    -- | The jQuery UI 1.8 Javascript file.
    urlJqueryUiJs :: a -> Either (Route a) Text
    urlJqueryUiJs _ = Right "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"

    -- | The jQuery UI 1.8 CSS file; defaults to cupertino theme.
    urlJqueryUiCss :: a -> Either (Route a) Text
    urlJqueryUiCss _ = Right $ googleHostedJqueryUiCss "cupertino"

    -- | jQuery UI time picker add-on.
    urlJqueryUiDateTimePicker :: a -> Either (Route a) Text
    urlJqueryUiDateTimePicker _ = Right "http://github.com/gregwebs/jquery.ui.datetimepicker/raw/master/jquery.ui.datetimepicker.js"

jqueryDayField :: (RenderMessage master FormMessage, YesodJquery master) => JqueryDaySettings -> Field sub master Day
jqueryDayField jds = Field
    { fieldParse = parseHelper $ maybe
                  (Left MsgInvalidDay)
                  Right
              . readMay
              . unpack
    , fieldView = \theId name attrs val isReq -> do
        toWidget [shamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="date" :isReq:required="" value="#{showVal val}">
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        toWidget [julius|
$(function(){
    var i = document.getElementById("#{rawJS theId}");
    if (i.type != "date") {
        $(i).datepicker({
            dateFormat:'yy-mm-dd',
            changeMonth:#{jsBool $ jdsChangeMonth jds},
            changeYear:#{jsBool $ jdsChangeYear jds},
            numberOfMonths:#{rawJS $ mos $ jdsNumberOfMonths jds},
            yearRange:#{toJSON $ jdsYearRange jds}
        });
    }
});
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . show)
    jsBool True = toJSON True
    jsBool False = toJSON False
    mos (Left i) = show i
    mos (Right (x, y)) = concat
        [ "["
        , show x
        , ","
        , show y
        , "]"
        ]

jqueryAutocompleteField :: (RenderMessage master FormMessage, YesodJquery master)
                        => Route master -> Field sub master Text
jqueryAutocompleteField src = Field
    { fieldParse = parseHelper $ Right
    , fieldView = \theId name attrs val isReq -> do
        toWidget [shamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required="" value="#{either id id val}" .autocomplete>
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        toWidget [julius|
$(function(){$("##{rawJS theId}").autocomplete({source:"@{src}",minLength:2})});
|]
    , fieldEnctype = UrlEncoded
    }

addScript' :: (master -> Either (Route master) Text) -> GWidget sub master ()
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
