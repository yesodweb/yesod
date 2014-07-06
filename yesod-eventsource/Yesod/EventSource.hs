{-# LANGUAGE OverloadedStrings #-}
-- | This module contains everything that you need to support
-- server-sent events in Yesod applications.
module Yesod.EventSource
  ( repEventSource
  , pollingEventSource
  , ioToRepEventSource
  , EventSourcePolyfill(..)
  ) where

import Blaze.ByteString.Builder (Builder)
import Control.Monad (when)
import Data.Functor ((<$>))
import Data.Monoid (mappend, mempty)
import Yesod.Core
import qualified Data.Conduit as C
import qualified Network.Wai as W
import qualified Network.Wai.EventSource as ES
import qualified Network.Wai.EventSource.EventStream as ES



-- | (Internal) Find out the request's 'EventSourcePolyfill' and
-- set any necessary headers.
prepareForEventSource :: MonadHandler m => m EventSourcePolyfill
prepareForEventSource = do
  reqWith <- lookup "X-Requested-With" . W.requestHeaders <$> waiRequest
  let polyfill | reqWith == Just "XMLHttpRequest" = Remy'sESPolyfill
               | otherwise                        = NoESPolyfill
  addHeader "Cache-Control" "no-cache" -- extremely important!
  return polyfill


-- | (Internal) Source with a event stream content-type.
respondEventStream :: C.Source (HandlerT site IO) (C.Flush Builder)
                   -> HandlerT site IO TypedContent
respondEventStream = respondSource "text/event-stream"


-- | Returns a Server-Sent Event stream from a 'C.Source' of
-- 'ES.ServerEvent'@s@.  The HTTP socket is flushed after every
-- event.  The connection is closed either when the 'C.Source'
-- finishes outputting data or a 'ES.CloseEvent' is outputted,
-- whichever comes first.
repEventSource :: (EventSourcePolyfill -> C.Source (HandlerT site IO) ES.ServerEvent)
               -> HandlerT site IO TypedContent
repEventSource src =
  prepareForEventSource >>=
  respondEventStream . sourceToSource . src

-- | Convert a ServerEvent source into a Builder source of serialized
-- events.
sourceToSource :: Monad m => C.Source m ES.ServerEvent -> C.Source m (C.Flush Builder)
sourceToSource src =
    src C.$= C.awaitForever eventToFlushBuilder
  where
    eventToFlushBuilder event =
        case ES.eventToBuilder event of
            Nothing -> return ()
            Just x -> C.yield (C.Chunk x) >> C.yield C.Flush


-- | Return a Server-Sent Event stream given a 'HandlerT' action
-- that is repeatedly called.  A state is threaded for the action
-- so that it may avoid using @IORefs@.  The @HandlerT@ action
-- may sleep or block while waiting for more data.  The HTTP
-- socket is flushed after every list of simultaneous events.
-- The connection is closed as soon as an 'ES.CloseEvent' is
-- outputted, after which no other events are sent to the client.
pollingEventSource :: s
                   -> (EventSourcePolyfill -> s -> HandlerT site IO ([ES.ServerEvent], s))
                   -> HandlerT site IO TypedContent
pollingEventSource initial act = do
  polyfill <- prepareForEventSource
  let -- Get new events to be sent.
      getEvents s = do
        (evs, s') <- lift (act polyfill s)
        case evs of
          [] -> getEvents s'
          _  -> do
            let (builder, continue) = joinEvents evs mempty
            C.yield (C.Chunk builder)
            C.yield C.Flush
            when continue (getEvents s')

      -- Join all events in a single Builder.  Returns @False@
      -- when we the connection should be closed.
      joinEvents (ev:evs) acc =
        case ES.eventToBuilder ev of
          Just b  -> joinEvents evs (acc `mappend` b)
          Nothing -> (fst $ joinEvents [] acc, False)
      joinEvents [] acc = (acc, True)

  respondEventStream (getEvents initial)


-- | Return a Server-Sent Event stream given an @IO@ action that
-- is repeatedly called.  A state is threaded for the action so
-- that it may avoid using @IORefs@.  The @IO@ action may sleep
-- or block while waiting for more data.  The HTTP socket is
-- flushed after every list of simultaneous events.  The
-- connection is closed as soon as an 'ES.CloseEvent' is
-- outputted, after which no other events are sent to the client.
ioToRepEventSource :: s
                   -> (EventSourcePolyfill -> s -> IO ([ES.ServerEvent], s))
                   -> HandlerT site IO TypedContent
ioToRepEventSource initial act = pollingEventSource initial act'
  where act' p s = liftIO (act p s)


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
