{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Provide the user with a rich text editor.
module Yesod.Form.Nic
    ( YesodNic (..)
    , nicHtmlField
    ) where

import Yesod.Core
import Yesod.Form
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Hamlet (shamlet)
import Text.Julius (julius, rawJS)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, pack)
import Data.Maybe (listToMaybe)

class Yesod a => YesodNic a where
    -- | NIC Editor Javascript file.
    urlNicEdit :: a -> Either (Route a) Text
    urlNicEdit _ = Right "http://js.nicedit.com/nicEdit-latest.js"

nicHtmlField :: YesodNic site => Field (HandlerT site IO) Html
nicHtmlField = Field
    { fieldParse = \e _ -> return . Right . fmap (preEscapedToMarkup . sanitizeBalance) . listToMaybe $ e
    , fieldView = \theId name attrs val _isReq False -> do
        toWidget [shamlet|
$newline never
    <textarea id="#{theId}" *{attrs} name="#{name}" .html>#{showVal val}
|]
        addScript' urlNicEdit
        master <- getYesod
        toWidget $
          case jsLoader master of
            BottomOfHeadBlocking -> [julius|
bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("#{rawJS theId}")});
|]
            _ -> [julius|
(function(){new nicEditor({fullPanel:true}).panelInstance("#{rawJS theId}")})();
|]
    , fieldEnctype = UrlEncoded
    , fieldHidden = False
    }
  where
    showVal = either id (pack . renderHtml)

addScript' :: (MonadWidget m, HandlerSite m ~ site)
           => (site -> Either (Route site) Text)
           -> m ()
addScript' f = do
    y <- getYesod
    addScriptEither $ f y
