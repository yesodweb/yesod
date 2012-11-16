{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME remove
{-# LANGUAGE CPP #-}
-- | Provide the user with a rich text editor.
module Yesod.Form.Nic
    ( YesodNic (..)
    , nicHtmlField
    ) where

import Yesod.Handler
import Yesod.Core (Route, ScriptLoadPosition(..), jsLoader, Yesod)
import Yesod.Form
import Yesod.Widget
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Hamlet (Html, shamlet)
import Text.Julius (julius, rawJS)
#if MIN_VERSION_blaze_html(0, 5, 0)
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html.Renderer.String (renderHtml)
#define preEscapedText preEscapedToMarkup
#else
import Text.Blaze (preEscapedText)
import Text.Blaze.Renderer.String (renderHtml)
#endif
import Data.Text (Text, pack)
import Data.Maybe (listToMaybe)

class Yesod a => YesodNic a where
    -- | NIC Editor Javascript file.
    urlNicEdit :: a -> Either (Route a) Text
    urlNicEdit _ = Right "http://js.nicedit.com/nicEdit-latest.js"

nicHtmlField :: YesodNic master => Field sub master Html
nicHtmlField = Field
    { fieldParse = \e _ -> return . Right . fmap (preEscapedText . sanitizeBalance) . listToMaybe $ e
    , fieldView = \theId name attrs val _isReq -> do
        toWidget [shamlet|
$newline never
    <textarea id="#{theId}" *{attrs} name="#{name}" .html>#{showVal val}
|]
        addScript' urlNicEdit
        master <- lift getYesod
        toWidget $
          case jsLoader master of
            BottomOfHeadBlocking -> [julius|
bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("#{rawJS theId}")});
|]
            _ -> [julius|
(function(){new nicEditor({fullPanel:true}).panelInstance("#{rawJS theId}")})();
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . renderHtml)

addScript' :: (y -> Either (Route y) Text) -> GWidget sub y ()
addScript' f = do
    y <- lift getYesod
    addScriptEither $ f y
