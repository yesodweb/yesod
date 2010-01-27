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
import Control.Monad (foldM)
import Data.ByteString.Lazy (toChunks)

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
        [ (TypeJson, cs $ unJsonDoc $ cs ho)
        , (TypeHtml, tempToContent t ho f)
        ]
newtype NoSuchTemplate = NoSuchTemplate String
    deriving (Show, Typeable)
instance Exception NoSuchTemplate

tempToContent :: Template
              -> HtmlObject
              -> (HtmlObject -> Template -> IO Template)
              -> Content
tempToContent t ho f = ioTextToContent $ fmap render $ f ho t

ioTextToContent :: IO Text -> Content
ioTextToContent iotext =
    Content $ \f a -> iotext >>= foldM f a . toChunks . cs

data TemplateFile = TemplateFile FilePath HtmlObject
instance HasReps TemplateFile where
    chooseRep (TemplateFile fp (Mapping m)) _ = do
        t <- fmap newSTMP $ readFile fp
        let t' = setManyAttrib m t :: Template
        return (TypeHtml, cs $ render t')
    chooseRep _ _ = error "Please fix type of TemplateFile"

loadTemplateGroup :: FilePath -> IO TemplateGroup
loadTemplateGroup = directoryGroupRecursiveLazy
