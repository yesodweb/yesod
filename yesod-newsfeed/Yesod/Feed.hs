-------------------------------------------------------------------------------
--
-- Module        : Yesod.Feed
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : Stable
-- Portability   : Portable
--
-- Generic Feed and Feed Entry data types that can be used as either an
-- Rss feed or an Atom feed (or both, or other).
--
-- Atom spec: <http://en.wikipedia.org/wiki/Atom_(standard)>
-- Rss spec:  <http://www.rssboard.org/rss-specification>
--
-------------------------------------------------------------------------------
module Yesod.Feed
    ( newsFeed
    , RepAtomRss (..)
    , module Yesod.FeedTypes
    ) where

import Yesod.FeedTypes
import Yesod.AtomFeed
import Yesod.RssFeed
import Yesod.Content (HasReps (chooseRep), typeAtom, typeRss)
import Yesod.Handler (Route, GGHandler)

data RepAtomRss = RepAtomRss RepAtom RepRss
instance HasReps RepAtomRss where
    chooseRep (RepAtomRss (RepAtom a) (RepRss r)) = chooseRep
        [ (typeAtom, a)
        , (typeRss, r)
        ]
newsFeed :: Monad mo => Feed (Route master) -> GGHandler sub master mo RepAtomRss
newsFeed f = do
    a <- atomFeed f
    r <- rssFeed f
    return $ RepAtomRss a r
