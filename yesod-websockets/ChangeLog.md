## 0.3.0.3
* Removed the use of the deprecated forkPingThread and replaced it with the recommended withPingThread. [#1700](https://github.com/yesodweb/yesod/pull/1700)

## 0.3.0.2

* `sendClose` and `sendPing` correctly find the `Connection` from `WebSocketsT`

  Previously, these types were incorrectly typed to look for the `Connection` on
  the `m` of `WebSocketsT m ()`, and not via `WebSocketsT` itself. Because this
  made them practically impossible to use, this change is unlikely to break any
  existing code.

## 0.3.0.1

* Minor cabal file improvements

## 0.3.0

* Upgrade to yesod-core 1.6

## 0.2.6

* Fix warnings

## 0.2.5

* Allow to start websockets with custom ConnectionOptions with `webSocketsOptions` and `webSocketsOptionsWith`

## 0.2.4.1

* Support for websockets 0.10

## 0.2.3

* `receiveDataMessageE` and `sendDataMessageE`

## 0.2.2

* Add exceptional websocket commands [#772](https://github.com/yesodweb/yesod/pull/772)
