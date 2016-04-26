{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Core.FormHelpers
    (
      httpLink,
      httpLinkParams,
      buttonLink,
      buttonLinkParams,
      link,
      linkParams,
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Yesod.Routes.Class
import Control.Monad.Trans.Resource (MonadResource (..), MonadThrow (..))
import Yesod.Core.Widget
import Yesod.Core.Handler
import Text.Blaze (ToMarkup)
import Data.String (IsString)
import Text.Lucius
import Data.Text

httpLinkParams :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, ToWidget site c) =>
                  (Text -> c)
                  -> [(Text, Text)]
                  -> Text
                  -> Route site
                  -> Text
                  -> WidgetT site m ()
httpLinkParams buttonCSS params methodType path text = do
    request <- getRequest
    buttonClass <- newIdent
    toWidget $ formMethodWrapper
      [whamlet|
        $maybe token <- reqToken request
          <input type=hidden name=#{defaultCsrfParamName} value=#{token}>
          <button class=#{buttonClass}> #{text}
      |]
    toWidget $
      buttonCSS buttonClass
  where
    formMethodWrapper contents = 
      [whamlet|
        $if isFormSupported
          <form action=@?{(path, queryString)} method=#{methodType}>
            ^{contents}
        $else
          <form action=@?{(path, queryString)}>
            ^{contents}
      |]
    queryString
      | isFormSupported = params
      | otherwise = [("_method", methodType)] ++ params
    isFormSupported = toUpper methodType == "GET" || toUpper methodType == "POST"

httpLink :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, ToWidget site c) =>
            (Text -> c)
            -> Text
            -> Route site
            -> Text
            -> WidgetT site m ()
httpLink buttonCSS = httpLinkParams buttonCSS []

buttonLinkParams :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => [(Text, Text)]
                 -> Text
                 -> Route site
                 -> Text
                 -> WidgetT site m ()
buttonLinkParams params = httpLinkParams style params
  where
    style ident =
      [lucius|
        .#{ident} {
          background: none;
          border: none;
          padding: 0;
          color: #069;
          text-decoration: underline;
          cursor: pointer;
        }
      |]

buttonLink :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
              Text
              -> Route site
              -> Text
              -> WidgetT site m ()
buttonLink = buttonLinkParams []

linkParams :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
              [(Text, Text)]
              -> Text
              -> Route site
              -> Text
              -> WidgetT site m ()
linkParams params = httpLinkParams style params
  where
    style =
      [lucius|
      |]

link :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
        Text
        -> Route site
        -> Text
        -> WidgetT site m ()
link = linkParams []
