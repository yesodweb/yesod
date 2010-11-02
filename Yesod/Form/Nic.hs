{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Provide the user with a rich text editor.
module Yesod.Form.Nic
    ( YesodNic (..)
    , nicHtmlField
    , maybeNicHtmlField
    ) where

import Yesod.Handler
import Yesod.Form.Core
import Yesod.Hamlet
import Yesod.Widget
import Text.HTML.SanitizeXSS (sanitizeBalance)

import Yesod.Internal (lbsToChars)

class YesodNic a where
    -- | NIC Editor Javascript file.
    urlNicEdit :: a -> Either (Route a) String
    urlNicEdit _ = Right "http://js.nicedit.com/nicEdit-latest.js"

nicHtmlField :: (IsForm f, FormType f ~ Html, YesodNic (FormMaster f))
             => FormFieldSettings -> Maybe Html -> f
nicHtmlField = requiredFieldHelper nicHtmlFieldProfile

maybeNicHtmlField
    :: (IsForm f, FormType f ~ Maybe Html, YesodNic (FormMaster f))
    => FormFieldSettings -> Maybe (FormType f) -> f
maybeNicHtmlField = optionalFieldHelper nicHtmlFieldProfile

nicHtmlFieldProfile :: YesodNic y => FieldProfile sub y Html
nicHtmlFieldProfile = FieldProfile
    { fpParse = Right . preEscapedString . sanitizeBalance
    , fpRender = lbsToChars . renderHtml
    , fpWidget = \theId name val _isReq -> do
        addHtml [$hamlet|%textarea.html#$theId$!name=$name$ $val$|]
        addScript' urlNicEdit
        addJulius [$julius|bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("%theId%")});|]
    }

addScript' :: (y -> Either (Route y) String) -> GWidget sub y ()
addScript' f = do
    y <- liftHandler getYesod
    addScriptEither $ f y
