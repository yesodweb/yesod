{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Core.FormHelpers
    (
      FormStyle,
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

data FormStyle = ButtonForm | LinkForm

httpLink :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => FormStyle -> Text -> Route site -> Text -> WidgetT site m ()
httpLink formType methodType path text = do
    request <- getRequest
    toWidget $ formMethodWrapper
      [whamlet|
        $maybe token <- reqToken request
          <input type=hidden name=#{defaultCsrfParamName} value=#{token}>
        $case formType
          $of LinkForm
            <button class=buttonLink> #{text}
          $of ButtonForm
            <button> #{text}
      |]
    toWidget
      [lucius|
        .buttonLink {
          background: none;
          border: none; 
          padding: 0;
          color: #069;
          text-decoration: underline;
          cursor: pointer;
        }
      |]
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
buttonLink = httpLink ButtonForm

link :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => Text -> Route site -> Text -> WidgetT site m ()
link = httpLink LinkForm
