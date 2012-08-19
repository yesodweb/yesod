{-# LANGUAGE OverloadedStrings #-}
-- | This module contains everything that you need to support
-- server-sent events in Yesod applications.
module Yesod.EventSource
  ( RepEventSource
  , repEventSource
  , ioToRepEventSource
  , EventSourcePolyfill(..)
  ) where

import Blaze.ByteString.Builder (Builder)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import Data.Monoid (mconcat)
import Yesod.Content
import Yesod.Core
import qualified Data.Conduit as C
import qualified Network.Wai as W
import qualified Network.Wai.EventSource as ES
import qualified Network.Wai.EventSource.EventStream as ES



-- | Phantom type used for 'Handler'@s@ that are @EventSources@
-- (e.g. 'repEventSource' and 'ioToRepEventSource').
newtype RepEventSource =
  RepEventSource (C.Source (C.ResourceT IO) (C.Flush Builder))

instance HasReps RepEventSource where
  chooseRep (RepEventSource src) =
    const $ return ("text/event-stream", ContentSource src)


-- | (Internal) Find out the request's 'EventSourcePolyfill' and
-- set any necessary headers.
prepareForEventSource :: GHandler sub master EventSourcePolyfill
prepareForEventSource = do
  reqWith <- lookup "X-Requested-With" . W.requestHeaders <$> waiRequest
  let polyfill | reqWith == Just "XMLHttpRequest" = Remy'sESPolyfill
               | otherwise                        = NoESPolyfill
  setHeader "Cache-Control" "no-cache" -- extremely important!
  return polyfill


-- | Returns a Server-Sent Event stream from a 'C.Source' of
-- 'ES.ServerEvent'@s@.  The HTTP socket is flushed after every
-- event.  The connection is closed either when the 'C.Source'
-- finishes outputting data or a 'ES.CloseEvent' is outputted,
-- whichever comes first.
repEventSource :: (EventSourcePolyfill -> C.Source (C.ResourceT IO) ES.ServerEvent)
               -> GHandler sub master RepEventSource
repEventSource src = RepEventSource . ES.sourceToSource . src <$> prepareForEventSource


-- | Return a Server-Sent Event stream given an @IO@ action that
-- is repeatedly called.  An state is threaded for the action so
-- that it may avoid using @IORefs@.  The @IO@ action may sleep
-- or block while waiting for more data.  The HTTP socket is
-- flushed after every list of simultaneous events.  The
-- connection is closed as soon as an 'ES.CloseEvent' is
-- outputted, after which no other events are sent to the client.
ioToRepEventSource :: s
                   -> (EventSourcePolyfill -> s -> IO ([ES.ServerEvent], s))
                   -> GHandler sub master RepEventSource
ioToRepEventSource initial act = do
  polyfill <- prepareForEventSource
  let -- Get new events to be sent.
      getEvents s = do
        (evs, s') <- liftIO (act polyfill s)
        let (builder, continue) = joinEvents evs []
        C.yield (C.Chunk builder)
        C.yield C.Flush
        when continue (getEvents s')

      -- Join all events in a single Builder.  Returns @False@
      -- when we the connection should be closed.
      joinEvents (ev:evs) acc =
        case ES.eventToBuilder ev of
          Just b  -> joinEvents evs (b:acc)
          Nothing -> (fst $ joinEvents [] acc, False)
      joinEvents [] acc = (mconcat (reverse acc), True)

  return $ RepEventSource $ getEvents initial


-- | Which @EventSource@ polyfill was detected (if any).
data EventSourcePolyfill =
    NoESPolyfill
    -- ^ We didn't detect any @EventSource@ polyfill that we know.
  | Remy'sESPolyfill
    -- ^ See
    -- <https://github.com/remy/polyfills/blob/master/EventSource.js>.
    -- In order to support Remy\'s polyfill, your server needs to
    -- explicitly close the connection from time to
    -- time--browsers such as IE7 will not show any event until
    -- the connection is closed.
    deriving (Eq, Ord, Show, Enum)
