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
import Yesod.Core
import Data.Conduit
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
respondEventStream :: ConduitT () (Flush Builder) (HandlerFor site) ()
                   -> HandlerFor site TypedContent
respondEventStream = respondSource "text/event-stream"


-- | Returns a Server-Sent Event stream from a 'Source' of
-- 'ES.ServerEvent'@s@.  The HTTP socket is flushed after every
-- event.  The connection is closed either when the 'Source'
-- finishes outputting data or a 'ES.CloseEvent' is outputted,
-- whichever comes first.
repEventSource :: (EventSourcePolyfill -> ConduitT () ES.ServerEvent (HandlerFor site) ())
               -> HandlerFor site TypedContent
repEventSource src =
  prepareForEventSource >>=
  respondEventStream . sourceToSource . src

-- | Convert a ServerEvent source into a Builder source of serialized
-- events.
sourceToSource
  :: Monad m
  => ConduitT () ES.ServerEvent m ()
  -> ConduitT () (Flush Builder) m ()
sourceToSource src =
    src .| awaitForever eventToFlushBuilder
  where
    eventToFlushBuilder event =
        case ES.eventToBuilder event of
            Nothing -> return ()
            Just x -> yield (Chunk x) >> yield Flush


-- | Return a Server-Sent Event stream given a 'HandlerFor' action
-- that is repeatedly called.  A state is threaded for the action
-- so that it may avoid using @IORefs@.  The @HandlerFor@ action
-- may sleep or block while waiting for more data.  The HTTP
-- socket is flushed after every list of simultaneous events.
-- The connection is closed as soon as an 'ES.CloseEvent' is
-- outputted, after which no other events are sent to the client.
pollingEventSource :: s
                   -> (EventSourcePolyfill -> s -> HandlerFor site ([ES.ServerEvent], s))
                   -> HandlerFor site TypedContent
pollingEventSource initial act = do
  polyfill <- prepareForEventSource
  let -- Get new events to be sent.
      getEvents s = do
        (evs, s') <- lift (act polyfill s)
        case evs of
          [] -> getEvents s'
          _  -> do
            let (builder, continue) = joinEvents evs mempty
            yield (Chunk builder)
            yield Flush
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
                   -> HandlerFor site TypedContent
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
