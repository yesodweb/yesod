## yesod-eventsource

It's easy to send an event from an HTTP client to a server:
just send an HTTP request.  However, sending events from the
server to the client requires more sophisticated approaches.
[Server-sent events](http://www.w3.org/TR/eventsource/) are a
standardized way of pushing events from the server to the
client.

This package allows your Yesod application to easily send
server-sent events.  On the client side, you may use the
`EventSource` JavaScript object on browsers that support it
(https://developer.mozilla.org/en-US/docs/Server-sent_events/EventSource)
or a polyfill for browsers that don't (we support Remy's
polyfill out-of-the-box, although that requires you to
explicitly support it).
