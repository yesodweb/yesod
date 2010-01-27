{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Template
    ( YesodTemplate (..)
    , template
    , NoSuchTemplate
    , Template
    , TemplateGroup
    , TemplateFile (..)
    , setAttribute
    , loadTemplateGroup
    ) where

import Data.Object.Html
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Data.Object.Text (Text)
import Text.StringTemplate
import Data.Object.Json
import Web.Mime
import Yesod.Response
import Yesod.Yesod
import Yesod.Handler

type Template = StringTemplate Text
type TemplateGroup = STGroup Text

class Yesod y => YesodTemplate y where
    getTemplateGroup :: y -> TemplateGroup

getTemplateGroup' :: YesodTemplate y => Handler y TemplateGroup
getTemplateGroup' = getTemplateGroup `fmap` getYesod

template :: YesodTemplate y
         => String -- ^ template name
         -> HtmlObject -- ^ object
         -> (HtmlObject -> Template -> IO Template)
         -> Handler y ChooseRep
template tn ho f = do
    tg <- getTemplateGroup'
    t <- case getStringTemplate tn tg of
            Nothing -> failure $ NoSuchTemplate tn
            Just x -> return x
    return $ chooseRep
        [ (TypeHtml, tempToContent t ho f)
        , (TypeJson, cs $ unJsonDoc $ cs ho)
        ]
newtype NoSuchTemplate = NoSuchTemplate String
    deriving (Show, Typeable)
instance Exception NoSuchTemplate

tempToContent :: Template
              -> HtmlObject
              -> (HtmlObject -> Template -> IO Template)
              -> Content
tempToContent t ho f = ioTextToContent $ fmap render $ f ho t

data TemplateFile = TemplateFile FilePath HtmlObject
instance HasReps TemplateFile where
    chooseRep (TemplateFile fp (Mapping m)) _ = do
        t <- fmap newSTMP $ readFile fp
        let t' = setManyAttrib m t :: Template
        return (TypeHtml, cs $ render t')
    chooseRep _ _ = error "Please fix type of TemplateFile"

loadTemplateGroup :: FilePath -> IO TemplateGroup
loadTemplateGroup = directoryGroupRecursiveLazy
