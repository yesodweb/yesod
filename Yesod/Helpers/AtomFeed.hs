{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Yesod.Helpers.AtomFeed
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Generating atom news feeds.
--
---------------------------------------------------------

module Yesod.Helpers.AtomFeed
    ( AtomFeed (..)
    , AtomFeedEntry (..)
    , AtomFeedResponse (..)
    , atomFeed
    ) where

import Yesod
import Data.Time.Clock (UTCTime)
-- FIXME import Web.Encodings (formatW3)
import Data.Convertible.Text

data AtomFeedResponse = AtomFeedResponse AtomFeed Approot

atomFeed :: Yesod y => AtomFeed -> Handler y AtomFeedResponse
atomFeed f = do
    y <- getYesod
    return $ AtomFeedResponse f $ approot y

data AtomFeed = AtomFeed
    { atomTitle :: String
    , atomLinkSelf :: Location
    , atomLinkHome :: Location
    , atomUpdated :: UTCTime
    , atomEntries :: [AtomFeedEntry]
    }
instance HasReps AtomFeedResponse where
    chooseRep = defChooseRep
        [ (TypeAtom, return . cs)
        ]

data AtomFeedEntry = AtomFeedEntry
    { atomEntryLink :: Location
    , atomEntryUpdated :: UTCTime
    , atomEntryTitle :: String
    , atomEntryContent :: HtmlContent
    }

instance ConvertSuccess AtomFeedResponse Content where
    convertSuccess = error "FIXME" -- cs . (cs :: Html -> XmlDoc) . cs
{- FIXME
instance ConvertSuccess AtomFeedResponse Html where
    convertSuccess (AtomFeedResponse f ar) =
        Tag "feed" [("xmlns", "http://www.w3.org/2005/Atom")] $ HtmlList
          [ Tag "title" [] $ cs $ atomTitle f
          , EmptyTag "link" [ ("rel", "self")
                            , ("href", showLocation ar $ atomLinkSelf f)
                            ]
          , EmptyTag "link" [ ("href", showLocation ar $ atomLinkHome f)
                            ]
          , Tag "updated" [] $ cs $ formatW3 $ atomUpdated f
          , Tag "id" [] $ cs $ showLocation ar $ atomLinkHome f
          , HtmlList $ map cs $ zip (atomEntries f) $ repeat ar
        ]

instance ConvertSuccess (AtomFeedEntry, Approot) Html where
    convertSuccess (e, ar) = Tag "entry" [] $ HtmlList
        [ Tag "id" [] $ cs $ showLocation ar $ atomEntryLink e
        , EmptyTag "link" [("href", showLocation ar $ atomEntryLink e)]
        , Tag "updated" [] $ cs $ formatW3 $ atomEntryUpdated e
        , Tag "title" [] $ cs $ atomEntryTitle e
        , Tag "content" [("type", "html")] $ cdata $ atomEntryContent e
        ]
-}
