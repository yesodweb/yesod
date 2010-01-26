-- FIXME this whole module needs to be rethought
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Template
    ( HasTemplateGroup (..)
    , template
    , NoSuchTemplate
    , TemplateGroup
    , Template (..)
    , TemplateFile (..)
    ) where

import Data.Object.Html
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Failure
import Data.Object.Text (Text)
import Text.StringTemplate
import Data.Object.Json
import Web.Mime
import Yesod.Response

type TemplateGroup = STGroup Text

class HasTemplateGroup a where
    getTemplateGroup :: a TemplateGroup

template :: (MonadFailure NoSuchTemplate t, HasTemplateGroup t)
         => String -- ^ template name
         -> String -- ^ object name
         -> HtmlObject -- ^ object
         -> IO [(String, HtmlObject)] -- ^ template attributes
         -> t Template
template tn on o attrs = do
    tg <- getTemplateGroup
    t <- case getStringTemplate tn tg of
            Nothing -> failure $ NoSuchTemplate tn
            Just x -> return x
    return $ Template t on o attrs
newtype NoSuchTemplate = NoSuchTemplate String
    deriving (Show, Typeable)
instance Exception NoSuchTemplate

data Template = Template (StringTemplate Text)
                         String
                         HtmlObject
                         (IO [(String, HtmlObject)])
instance HasReps Template where
    chooseRep = defChooseRep [ (TypeHtml,
              \(Template t name ho attrsIO) -> do
                attrs <- attrsIO
                return
                    $ cs
                    $ render
                    $ setAttribute name ho
                    $ setManyAttrib attrs t)
           , (TypeJson, \(Template _ _ ho _) ->
                            return $ cs $ unJsonDoc $ cs ho)
           ]

data TemplateFile = TemplateFile FilePath HtmlObject
instance HasReps TemplateFile where
    chooseRep = defChooseRep [ (TypeHtml,
              \(TemplateFile fp h) -> do
                    contents <- readFile fp
                    let t = newSTMP contents
                    return $ cs $ toString $ setAttribute "o" h t
             )
           , (TypeJson, \(TemplateFile _ ho) ->
                            return $ cs $ unJsonDoc $ cs ho)
           ]
