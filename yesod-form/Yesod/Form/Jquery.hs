{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some fields spiced up with jQuery UI.
module Yesod.Form.Jquery
    ( YesodJquery (..)
    , jqueryDayField
    , jqueryDatePickerDayField
    , jqueryAutocompleteField
    , jqueryAutocompleteField'
    , jqueryAutocompleteField''
    , googleHostedJqueryUiCss
    , JqueryDaySettings (..)
    , Default (..)
    , jqfs
    ) where

import Yesod.Core
import Yesod.Form
import Data.Time (Day)
import Data.Default
import Text.Hamlet (shamlet)
import Text.Julius (julius, rawJS)
import Data.Text (Text, pack, unpack, append)
import Data.Monoid (mconcat)
import Data.List (find)

-- | Gets the Google hosted jQuery UI 1.8 CSS file with the given theme.
googleHostedJqueryUiCss :: Text -> Text
googleHostedJqueryUiCss theme = mconcat
    [ "//ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/"
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
    urlJqueryJs _ = Right "//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"

    -- | The jQuery UI 1.8 Javascript file.
    urlJqueryUiJs :: a -> Either (Route a) Text
    urlJqueryUiJs _ = Right "//ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"

    -- | The jQuery UI 1.8 CSS file; defaults to cupertino theme.
    urlJqueryUiCss :: a -> Either (Route a) Text
    urlJqueryUiCss _ = Right $ googleHostedJqueryUiCss "cupertino"

    -- | jQuery UI time picker add-on.
    urlJqueryUiDateTimePicker :: a -> Either (Route a) Text
    urlJqueryUiDateTimePicker _ = Right "http://github.com/gregwebs/jquery.ui.datetimepicker/raw/master/jquery.ui.datetimepicker.js"

jqueryDayField :: (RenderMessage site FormMessage, YesodJquery site) => JqueryDaySettings -> Field (HandlerT site IO) Day
jqueryDayField = flip jqueryDayField' "date"

-- | Use jQuery's datepicker as the underlying implementation.
--
-- Since 1.4.3
jqueryDatePickerDayField :: (RenderMessage site FormMessage, YesodJquery site) => JqueryDaySettings -> Field (HandlerT site IO) Day
jqueryDatePickerDayField = flip jqueryDayField' "text"

jqueryDayField' :: (RenderMessage site FormMessage, YesodJquery site) => JqueryDaySettings -> Text -> Field (HandlerT site IO) Day
jqueryDayField' jds inputType = Field
    { fieldParse = parseHelper $ maybe
                  (Left MsgInvalidDay)
                  Right
              . readMay
              . unpack
    , fieldView = \theId name attrs val isReq False -> do
        toWidget [shamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="#{inputType}" :isReq:required="" value="#{showVal val}">
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
    , fieldHidden = False
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

jqueryAutocompleteField :: (RenderMessage site FormMessage, YesodJquery site)
                        => Route site -> Field (HandlerT site IO) Text
jqueryAutocompleteField = jqueryAutocompleteField' 2

jqueryAutocompleteField' :: (RenderMessage site FormMessage, YesodJquery site)
                         => Int -- ^ autocomplete minimum length
                         -> Route site
                         -> Field (HandlerT site IO) Text
jqueryAutocompleteField' minLen = jqueryAutocompleteField'' minLen False

jqueryAutocompleteField'' :: (RenderMessage site FormMessage, YesodJquery site)
                         => Int -- ^ autocomplete minimum length
                         -> Bool -- ^ store value in hidden input
                         -> Route site
                         -> Field (HandlerT site IO) Text
jqueryAutocompleteField'' minLen hidden src = Field
    { fieldParse = parseHelper $ Right
    , fieldView = \theId name attrs val isReq False -> do
        let cls = find (\(c,_) -> c == "class") attrs
        let clsval c = case c of
                Just (_, clazz) -> append clazz " autocomplete"
                Nothing         -> pack "autocomplete"
        let attrs' = ("class", clsval cls) : (filter (\(c,_) -> c /= "class") attrs)
        toWidget [shamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs'} type="text" :isReq:required="" value="#{either id id val}">
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        case hidden of
            True  -> toWidget [julius|
$(function(){$("##{rawJS theId}").autocomplete({source:"@{src}",minLength:#{toJSON minLen},select:function(ev, ui){ev.preventDefault();$("##{rawJS theId}").val(ui.item.label);$("##{rawJS theId}_id").val(ui.item.value);}})});
|]
            False -> toWidget [julius|
$(function(){$("##{rawJS theId}").autocomplete({source:"@{src}",minLength:#{toJSON minLen}})});
|]
    , fieldEnctype = UrlEncoded
    , fieldHidden = False
    }


jqfs :: RenderMessage site msg
    => msg
    -> Text -- ^ base ID
    -> Bool -- ^ hidden field?
    -> Bool -- ^ bootstrap3?
    -> FieldSettings site
jqfs msg bid hidden bs =
    FieldSettings (SomeMessage msg) Nothing (id bid hidden) Nothing (cls hidden bs)
  where cls _ False    = []
        cls True True  = []
        cls False True = [("class", "form-control")]
        id :: Text -> Bool -> Maybe Text
        id baseid True  = Just (append baseid "_id")
        id baseid False = Just baseid

addScript' :: (HandlerSite m ~ site, MonadWidget m) => (site -> Either (Route site) Text) -> m ()
addScript' f = do
    y <- getYesod
    addScriptEither $ f y

addStylesheet' :: (MonadWidget m, HandlerSite m ~ site)
               => (site -> Either (Route site) Text)
               -> m ()
addStylesheet' f = do
    y <- getYesod
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
