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
import Yesod.Form
import Yesod.Widget
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Hamlet (Html, html)
import Text.Julius (julius)
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze (preEscapedText)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text, pack)
import Data.Maybe (listToMaybe)

class YesodNic a where
    -- | NIC Editor Javascript file.
    urlNicEdit :: a -> Either (Route a) Text
    urlNicEdit _ = Right "http://js.nicedit.com/nicEdit-latest.js"

nicHtmlField :: YesodNic master => Field sub master Html
nicHtmlField = Field
    { fieldParse = return . Right . fmap (preEscapedText . sanitizeBalance) . listToMaybe
    , fieldView = \theId name val _isReq -> do
        addHtml
#if __GLASGOW_HASKELL__ >= 700
                [html|
#else
                [$html|
#endif
    <textarea id="#{theId}" name="#{name}" .html>#{showVal val}
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
