{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME remove
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
import Text.Julius (julius)
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze (preEscapedText)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

class Yesod a => YesodNic a where
    -- | NIC Editor Javascript file.
    urlNicEdit :: a -> Either (Route a) Text
    urlNicEdit _ = Right "http://js.nicedit.com/nicEdit-latest.js"

nicHtmlField :: YesodNic master => Field sub master Html
nicHtmlField = Field
    { fieldParse = return . Right . fmap (preEscapedText . sanitizeBalance) . listToMaybe
    , fieldView = \theId name theClass val _isReq -> do
        addHtml [shamlet|
    <textarea id="#{theId}" :not (null theClass):class="#{T.intercalate " " theClass}" name="#{name}" .html>#{showVal val}
|]
        addScript' urlNicEdit
        master <- lift getYesod
        addJulius $
          case jsLoader master of
            BottomOfHeadBlocking -> [julius|
bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("#{theId}")});
|]
            _ -> [julius|
(function(){new nicEditor({fullPanel:true}).panelInstance("#{theId}")})();
|]
    }
  where
    showVal = either id (pack . renderHtml)

addScript' :: (y -> Either (Route y) Text) -> GWidget sub y ()
addScript' f = do
    y <- lift getYesod
    addScriptEither $ f y
