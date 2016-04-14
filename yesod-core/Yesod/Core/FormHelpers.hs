{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Core.FormHelpers
    (
      HttpMethod(..),
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

data HttpMethod = HttpGET | HttpPOST | HttpPATCH | HttpUPDATE | HttpDELETE
data FormStyle = ButtonForm | LinkForm

getMethodType :: (ToMarkup a, IsString a) => HttpMethod -> a
getMethodType method =
  case method of
    HttpGET -> "get"
    HttpPOST -> "post"
    HttpPATCH -> "patch"
    HttpUPDATE -> "patch"
    HttpDELETE -> "delete"

isStandardHttpMethod :: HttpMethod -> Bool
isStandardHttpMethod httpMethod =
  case httpMethod of
    HttpGET -> True
    HttpPOST -> True
    HttpPATCH -> False
    HttpUPDATE -> False
    HttpDELETE -> False

httpLink :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, ToMarkup a) => FormStyle -> HttpMethod -> Route site -> a -> WidgetT site m ()
httpLink formType methodType path text = do
  request <- getRequest
  let method = (getMethodType methodType) :: String
      standardHttpMethod = isStandardHttpMethod methodType
  toWidget
    [whamlet|
      <form action=@{path} method=#{method}>
        $maybe token <- reqToken request
          $if standardHttpMethod
            <input name=_method value=#{method}>
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

buttonLink :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, ToMarkup a) => HttpMethod -> Route site -> a -> WidgetT site m ()
buttonLink = httpLink ButtonForm

link :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, ToMarkup a) => HttpMethod -> Route site -> a -> WidgetT site m ()
link = httpLink LinkForm
