{-# LANGUAGE QuasiQuotes #-}
module Yesod.Form.Nic
    ( YesodNic (..)
    , nicHtmlField
    , maybeNicHtmlField
    ) where

import Yesod.Handler
import Yesod.Form
import Yesod.Form.Profiles
import Yesod.Hamlet
import Yesod.Widget
import qualified Data.ByteString.Lazy.UTF8 as U

class YesodNic a where
    -- | NIC Editor.
    urlNicEdit :: a -> Either (Route a) String
    urlNicEdit _ = Right "http://js.nicedit.com/nicEdit-latest.js"

nicHtmlField :: YesodNic y => FormFieldSettings -> FormletField sub y Html
nicHtmlField = requiredFieldHelper nicHtmlFieldProfile

maybeNicHtmlField :: YesodNic y => FormFieldSettings -> FormletField sub y (Maybe Html)
maybeNicHtmlField = optionalFieldHelper nicHtmlFieldProfile

nicHtmlFieldProfile :: YesodNic y => FieldProfile sub y Html
nicHtmlFieldProfile = FieldProfile
    { fpParse = Right . preEscapedString
    , fpRender = U.toString . renderHtml
    , fpHamlet = \theId name val _isReq -> [$hamlet|
%textarea.html#$theId$!name=$name$ $val$
|]
    , fpWidget = \name -> do
        addScript' urlNicEdit
        addJavaScript [$julius|bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("$name$")});|]
    }

addScript' :: (y -> Either (Route y) String) -> GWidget sub y ()
addScript' f = do
    y <- liftHandler getYesod
    addScriptEither $ f y
