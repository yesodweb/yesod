module Yesod.Urls
    ( newSetting
    , changeSetting
    , getSetting
      -- * Default library URLs
    , urlJqueryJs
    , urlJqueryUiJs
    , urlJqueryUiCss
    , urlJqueryUiDateTimePicker
    ) where

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class

newSetting :: a -> IORef a
newSetting = unsafePerformIO . newIORef

changeSetting :: MonadIO m => IORef a -> a -> m ()
changeSetting x = liftIO . writeIORef x

getSetting :: MonadIO m => IORef a -> m a
getSetting = liftIO . readIORef

-- | The Google-hosted jQuery 1.4.2 file.
urlJqueryJs :: IORef String
urlJqueryJs = newSetting
    "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"

-- | The Google-hosted jQuery UI 1.8.1 javascript file.
urlJqueryUiJs :: IORef String
urlJqueryUiJs = newSetting
    "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.1/jquery-ui.min.js"

-- | The Google-hosted jQuery UI 1.8.1 CSS file with cupertino theme.
urlJqueryUiCss :: IORef String
urlJqueryUiCss = newSetting
    "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.1/themes/cupertino/jquery-ui.css"

-- TODO - integrate with static helpers
urlJqueryUiDateTimePicker :: IORef String
urlJqueryUiDateTimePicker = newSetting
    "http://www.projectcodegen.com/jquery.ui.datetimepicker.js.txt"
