{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Hamlet
    ( hamletToContent
    , hamletToRepHtml
    , PageContent (..)
    , Hamlet
    , hamlet
    , HtmlContent (..)
    , HtmlObject
    )
    where

import Text.Hamlet
import Text.Hamlet.Monad (outputHtml)
import Yesod.Response
import Yesod.Handler
import Data.Convertible.Text
import Data.Object

data PageContent url = PageContent
    { pageTitle :: IO HtmlContent
    , pageHead :: Hamlet url IO ()
    , pageBody :: Hamlet url IO ()
    }

hamletToContent :: Hamlet (Routes y) IO () -> Handler y Content
hamletToContent h = do
    render <- getUrlRender
    return $ ContentEnum $ go render
  where
    go render iter seed = do
        res <- runHamlet h render seed $ iter' iter
        case res of
            Left x -> return $ Left x
            Right ((), x) -> return $ Right x
    iter' iter seed text = iter seed $ cs text

hamletToRepHtml :: Hamlet (Routes y) IO () -> Handler y RepHtml
hamletToRepHtml h = do
    c <- hamletToContent h
    return $ RepHtml c

instance Monad m => ConvertSuccess String (Hamlet url m ()) where
    convertSuccess = outputHtml . Unencoded . cs
instance Monad m
    => ConvertSuccess (Object String HtmlContent) (Hamlet url m ()) where
    convertSuccess (Scalar h) = outputHtml h
    convertSuccess (Sequence s) = template () where
        template = [$hamlet|
                %ul
                    $forall s' s
                        %li ^s^|]
        s' _ = return $ fromList $ map cs s
    convertSuccess (Mapping m) = template () where
        template :: Monad m => () -> Hamlet url m ()
        template = [$hamlet|
                %dl
                    $forall pairs pair
                        %dt $pair.key$
                        %dd ^pair.val^|]
        pairs _ = return $ fromList $ map go m
        go (k, v) = Pair (return $ cs k) $ cs v
instance ConvertSuccess String HtmlContent where
    convertSuccess = Unencoded . cs

data Pair url m = Pair { key :: m HtmlContent, val :: Hamlet url m () }

type HtmlObject = Object String HtmlContent

instance ConvertSuccess (Object String String) HtmlObject where
    convertSuccess = fmap cs
