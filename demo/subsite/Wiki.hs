{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Define the dispatch for a Wiki. You should probably start off by reading
-- WikiRoutes.
module Wiki
    ( module WikiRoutes
    ) where

import           Control.Monad       (unless)
import           Data.IORef.Lifted   (readIORef, atomicModifyIORef)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           WikiRoutes
import           Yesod

-- | A subsite needs to be an instance of YesodSubDispatch, which states how to
-- dispatch. By using constraints, we can make requirements of our master site.
-- In this example, we're saying that the master site must be an instance of
-- YesodWiki.
instance YesodWiki master => YesodSubDispatch Wiki (HandlerT master IO) where
    -- | This is all the TH magic for dispatch. WikiRoutes provides the
    -- resourcesWiki value automatically, and mkYesodSubDispatch will generate
    -- a dispatch function that will call out to the appropriate handler
    -- functions.
    yesodSubDispatch = $(mkYesodSubDispatch resourcesWiki)

-- | Helper type synonym to be used below.
type WikiHandler a = forall master. YesodWiki master
                  => HandlerT Wiki (HandlerT master IO) a

------------- Helper functions

-- | Get all of the content in the Wiki.
getContent :: WikiHandler (Map Texts Textarea)
getContent = getYesod >>= readIORef . wikiContent

-- | Put a single new value into the Wiki.
putContent :: Texts -> Textarea -> WikiHandler ()
putContent k v = do
    refMap <- wikiContent <$> getYesod
    atomicModifyIORef refMap $ \m -> (Map.insert k v m, ())

-- | Gets the homepage, which lists all of the pages available.
getWikiHomeR :: WikiHandler TypedContent
getWikiHomeR = do
    content <- getContent
    -- We use the new selectRep/provideRep functionality to provide either an
    -- HTML or JSON representation of the page. You could just as easily
    -- provide YAML, plain text, RSS, or anything else.
    selectRep $ do
        provideRep $ do
            -- We'll use toParent to convert Wiki routes into our master site
            -- routes.
            toParent <- getRouteToParent

            -- Run the master site's defaultLayout to style the page.
            lift $ defaultLayout
                [whamlet|
                    <p>This wiki has the following pages:
                    <ul>
                        $forall page <- Map.keys content
                            <li>
                                -- Notice the usage of toParent!
                                <a href=@{toParent $ WikiReadR page}>#{show page}
                |]
        -- You provide a JSON representation just by returning a JSON value.
        -- aeson's toJSON make it easy to convert a list of values into JSON.
        provideRep $ return $ toJSON $ Map.keys content

getWikiReadR :: Texts -> WikiHandler TypedContent
getWikiReadR page = do
    content <- getContent
    selectRep $ do
        provideRep $
            case Map.lookup page content of
                Nothing -> do
                    setMessage $ "Page does not exist, please create it."

                    -- We don't need to convert or lift here: we're using a
                    -- route from our subsite, and redirect lives in our
                    -- subsite.
                    redirect $ WikiEditR page
                Just t -> do
                    toParent <- getRouteToParent

                    -- Notice that we lift the canEditPage function from the
                    -- master site.
                    canEdit <- lift $ canEditPage page

                    lift $ defaultLayout
                        [whamlet|
                            <article>#{t}
                            $if canEdit
                                <p>
                                    <a href=@{toParent $ WikiEditR page}>Edit
                        |]
        provideRep $ return $ toJSON $
            case Map.lookup page content of
                -- Our HTML representation sends a redirect if the page isn't
                -- found, but our JSON representation just returns a JSON value
                -- instead.
                Nothing -> object ["error" .= ("Page not found" :: Text)]
                Just (Textarea t) -> object ["content" .= t]

getWikiEditR :: Texts -> WikiHandler Html
getWikiEditR page = do
    canEdit <- lift $ canEditPage page
    unless canEdit $ permissionDenied "You do not have permissions to edit this page."

    content <- getContent
    let form = renderTable
             $ areq textareaField "Content" (Map.lookup page content)

    -- We need to use lift here since the widget will be used below.
    -- Practically speaking, this means that we'll be rendering form messages
    -- using the master site's translation functions.
    ((res, widget), enctype) <- lift $ runFormPost form

    case res of
        FormSuccess t -> do
            putContent page t
            setMessage "Content updated"
            redirect $ WikiEditR page
        _ -> do
            toParent <- getRouteToParent
            lift $ defaultLayout
                [whamlet|
                    <p>
                        <a href=@{toParent $ WikiReadR page}>Read page
                    <form method=post action=@{toParent $ WikiEditR page} enctype=#{enctype}>
                        <table>
                            ^{widget}
                            <tr>
                                <td colspan=2>
                                    <button>Update page
                |]

postWikiEditR :: Texts -> WikiHandler Html
postWikiEditR = getWikiEditR
