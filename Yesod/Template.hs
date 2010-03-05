{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Template
    ( YesodTemplate (..)
    , NoSuchTemplate
    , Template
    , TemplateGroup
    , loadTemplateGroup
      -- * HTML templates
    , HtmlTemplate (..)
    , templateHtml
    , templateHtmlJson
    , setHtmlAttrib
    ) where

import Data.Object.Html
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Data.Object.Text (Text)
import Text.StringTemplate
import Yesod.Response
import Yesod.Yesod
import Yesod.Handler
import Control.Monad (join)
import Yesod.Request (Request, getRequest)

type Template = StringTemplate Text
type TemplateGroup = STGroup Text

class Yesod y => YesodTemplate y where
    getTemplateGroup :: y -> TemplateGroup
    defaultTemplateAttribs :: y -> Request -> HtmlTemplate
                           -> IO HtmlTemplate

getTemplateGroup' :: YesodTemplate y => Handler y TemplateGroup
getTemplateGroup' = getTemplateGroup `fmap` getYesod

newtype NoSuchTemplate = NoSuchTemplate String
    deriving (Show, Typeable)
instance Exception NoSuchTemplate

loadTemplateGroup :: FilePath -> IO TemplateGroup
loadTemplateGroup = directoryGroupRecursiveLazy

type TemplateName = String
newtype HtmlTemplate = HtmlTemplate { unHtmlTemplate :: Template }

-- | Return a result using a template generating HTML alone.
templateHtml :: YesodTemplate y
             => TemplateName
             -> (HtmlTemplate -> IO HtmlTemplate)
             -> Handler y RepHtml
templateHtml tn f = do
    tg <- getTemplateGroup'
    y <- getYesod
    t <- case getStringTemplate tn tg of
            Nothing -> failure $ InternalError $ show $ NoSuchTemplate tn
            Just x -> return x
    rr <- getRequest
    return $ RepHtml $ ioTextToContent
                     $ fmap (render . unHtmlTemplate)
                     $ join
                     $ fmap f
                     $ defaultTemplateAttribs y rr
                     $ HtmlTemplate t

setHtmlAttrib :: ConvertSuccess x HtmlObject
              => String -> x -> HtmlTemplate -> HtmlTemplate
setHtmlAttrib k v (HtmlTemplate t) =
    HtmlTemplate $ setAttribute k (toHtmlObject v) t

-- | Return a result using a template and 'HtmlObject' generating either HTML
-- or JSON output.
templateHtmlJson :: YesodTemplate y
                 => TemplateName
                 -> HtmlObject
                 -> (HtmlObject -> HtmlTemplate -> IO HtmlTemplate)
                 -> Handler y RepHtmlJson
templateHtmlJson tn ho f = do
    tg <- getTemplateGroup'
    y <- getYesod
    rr <- getRequest
    t <- case getStringTemplate tn tg of
            Nothing -> failure $ InternalError $ show $ NoSuchTemplate tn
            Just x -> return x
    return $ RepHtmlJson
            ( ioTextToContent
            $ fmap (render . unHtmlTemplate)
            $ join
            $ fmap (f ho)
            $ defaultTemplateAttribs y rr
            $ HtmlTemplate t
            )
            (hoToJsonContent ho)
