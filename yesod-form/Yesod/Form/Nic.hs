{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Provide the user with a rich text editor.
--
-- According to NIC editor homepage it is not actively maintained since June
-- 2012.  There is another better alternative — open sourced Summernote editor
-- released under MIT licence.  You can use Summernote in your Yesod forms via
-- separately distributed
-- <http://hackage.haskell.org/package/yesod-form-richtext yesod-form-richtext>
-- package.
module Yesod.Form.Nic
    ( YesodNic (..)
    , nicHtmlField
    ) where

import Yesod.Core
import Yesod.Form
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Julius (rawJS)
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
    , fieldView = \theId name attrs val _isReq -> do
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
    }
  where
    showVal = either id (pack . renderHtml)

addScript' :: (MonadWidget m, HandlerSite m ~ site)
           => (site -> Either (Route site) Text)
           -> m ()
addScript' f = do
    y <- getYesod
    addScriptEither $ f y
