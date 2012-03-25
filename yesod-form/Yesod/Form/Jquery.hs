{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME remove
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
import qualified Data.Text as T
import Data.Default
import Text.Hamlet (shamlet)
import Text.Julius (julius)
import Data.Text (Text, pack, unpack)
import Data.Monoid (mconcat)
import Yesod.Core (RenderMessage, SomeMessage (..))

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
    , fieldView = \theId name theClass val isReq -> do
        toWidget [shamlet|
<input id="#{theId}" name="#{name}" :not (null theClass):class="#{T.intercalate " " theClass}" type="date" :isReq:required="" value="#{showVal val}">
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        toWidget [julius|
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

jqueryAutocompleteField :: (RenderMessage master FormMessage, YesodJquery master)
                        => Route master -> Field sub master Text
jqueryAutocompleteField src = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name theClass val isReq -> do
        toWidget [shamlet|
<input id="#{theId}" name="#{name}" :not (null theClass):class="#{T.intercalate " " theClass}" type="text" :isReq:required="" value="#{either id id val}" .autocomplete>
|]
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        toWidget [julius|
$(function(){$("##{theId}").autocomplete({source:"@{src}",minLength:2})});
|]
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
