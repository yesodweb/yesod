{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Define our Wiki data type, routes, and the YesodWiki typeclass. Due to the
-- GHC stage restriction, the routes must be declared in a separate module from
-- our dispatch instance.
module WikiRoutes where

import           Control.Monad          (liftM)
import           Control.Monad.IO.Class (MonadIO)
import           Data.IORef             (IORef, newIORef)
import           Data.Map               (Map, empty)
import           Yesod

-- | Simple Wiki datatype: just store a Map from Wiki path to the contents of
-- the page.
data Wiki = Wiki
    { wikiContent :: IORef (Map Texts Textarea)
    }

-- | A typeclass that all master sites that want a Wiki must implement. A
-- master must be able to render form messages, as we use yesod-form for
-- processing user input.
class (RenderMessage master FormMessage, Yesod master) => YesodWiki master where
    -- | Write protection. By default, no protection.
    canEditPage :: Texts -> HandlerT master IO Bool
    canEditPage _ = return True

-- | Define our routes. We'll have a homepage that lists all of the pages, a
-- read route for reading a page, and an edit route.
mkYesodSubData "Wiki" [parseRoutes|
/ WikiHomeR GET
/read/*Texts WikiReadR GET
/edit/*Texts WikiEditR GET POST
|]

-- | A convenience function for creating an empty Wiki.
newWiki :: MonadIO m => m Wiki
newWiki = Wiki `liftM` liftIO (newIORef empty)
