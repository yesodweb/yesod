{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME remove
-- | Provide the user with a rich text editor.
module Yesod.Form.Nic
    ( YesodNic (..)
    , nicHtmlField
    ) where

import Yesod.Handler
import Yesod.Core (Route)
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

class YesodNic a where
    -- | NIC Editor Javascript file.
    urlNicEdit :: a -> Either (Route a) Text
    urlNicEdit _ = Right "http://js.nicedit.com/nicEdit-latest.js"

nicHtmlField :: YesodNic master => Field sub master Html
nicHtmlField = Field
    { fieldParse = return . Right . fmap (preEscapedText . sanitizeBalance) . listToMaybe
    , fieldView = \theId name theClass val _isReq -> do
        addHtml
#if __GLASGOW_HASKELL__ >= 700
                [shamlet|
#else
                [$shamlet|
#endif
    <textarea id="#{theId}" :not (null theClass):class="#{T.intercalate " " theClass}" name="#{name}" .html>#{showVal val}
|]
        addScript' urlNicEdit
        addJulius
#if __GLASGOW_HASKELL__ >= 700
                [julius|
#else
                [$julius|
#endif
bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("#{theId}")});
|]
    }
  where
    showVal = either id (pack . renderHtml)

addScript' :: (y -> Either (Route y) Text) -> GWidget sub y ()
addScript' f = do
    y <- lift getYesod
    addScriptEither $ f y
