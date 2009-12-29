{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Template
    ( HasTemplateGroup (..)
    , template
    , NoSuchTemplate
    , TemplateGroup
    ) where

import Data.Object.Html
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Failure
import Yesod.Rep
import Data.Object.Text (Text)
import Text.StringTemplate

type TemplateGroup = STGroup Text

class HasTemplateGroup a where
    getTemplateGroup :: a TemplateGroup

-- FIXME better home
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
