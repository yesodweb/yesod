module Yesod.Hamlet
    ( hamletToContent
    , hamletToRepHtml
    , PageContent (..)
    , Hamlet
    , hamlet
    , simpleContent
    , HtmlContent (..)
    )
    where

import Text.Hamlet
import Text.Hamlet.Monad (outputHtml)
import Yesod.Response
import Yesod.Handler
import Data.Text (pack)
import Data.Convertible.Text (cs)

data PageContent url = PageContent
    { pageTitle :: IO HtmlContent
    , pageHead :: Hamlet url IO ()
    , pageBody :: Hamlet url IO ()
    }

simpleContent :: String -> HtmlContent -> PageContent url
simpleContent title body = PageContent
    { pageTitle = return $ Unencoded $ pack title
    , pageHead = return ()
    , pageBody = outputHtml body
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
