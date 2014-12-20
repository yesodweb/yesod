## 1.4.7

SSL-only session security [#894](https://github.com/yesodweb/yesod/pull/894)

## 1.4.6.2

monad-control 1.0

## 1.4.6

Added the `Yesod.Core.Unsafe` module.

## 1.4.5

* `envClientSessionBackend`
* Add `MonadLoggerIO` instances (conditional on monad-logger 0.3.10 being used).

## 1.4.4.5

Support time 1.5

## 1.4.4.2

`neverExpires` uses dates one year in the future (instead of in 2037).

## 1.4.4.1

Improvements to etag/if-none-match support #868 #869

## 1.4.4

Add the `notModified` and `setEtag` functions.

## 1.4.3

Switch to mwc-random for token generation.
