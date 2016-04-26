{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Core.FormHelpers
    (
      httpLink,
      buttonLink,
      link
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

httpLink :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, ToWidget site c) => (Text -> c) -> Text -> Route site -> Text -> WidgetT site m ()
httpLink buttonCSS methodType path text = do
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
          <form action=@{path} method=#{methodType}>
            ^{contents}
        $else
          <form action=@?{(path, queryString)}>
            ^{contents}
      |]
    queryString
      | isFormSupported = []
      | otherwise = [("_method", methodType)]
    isFormSupported = toUpper methodType == "GET" || toUpper methodType == "POST"

buttonLink :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => Text -> Route site -> Text -> WidgetT site m ()
buttonLink = httpLink style
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

link :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => Text -> Route site -> Text -> WidgetT site m ()
link = httpLink
  [lucius|
  |]
