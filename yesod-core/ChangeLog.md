## 1.4.14

* Add CSRF protection functions and middleware based on HTTP cookies and headers [#1017](https://github.com/yesodweb/yesod/pull/1017)

## 1.4.13

* Add mkYesodGeneral, which allows creating sites with polymorphic type parameters [#1055](https://github.com/yesodweb/yesod/pull/1055)
* Add getsYesod function [#1042](https://github.com/yesodweb/yesod/pull/1042)
* Add IsString instance for WidgetT site m () [#1038](https://github.com/yesodweb/yesod/pull/1038)

## 1.4.12

* Don't show source location for logs that don't have that information [#1027](https://github.com/yesodweb/yesod/pull/1027)

## 1.4.11

* Expose `stripHandlerT` and `subHelper`

## 1.4.10

* Export log formatting [#1001](https://github.com/yesodweb/yesod/pull/1001)

## 1.4.9.1

* Deal better with multiple cookie headers

## 1.4.9

* Add simple authentication helpers [#962](https://github.com/yesodweb/yesod/pull/962)

## 1.4.8.3

* Use 307 redirect for cleaning paths and non-GET requests [#951](https://github.com/yesodweb/yesod/issues/951)

## 1.4.8.2

* Allow blaze-builder 0.4

## 1.4.8.1

* Bump upper bound on path-pieces

## 1.4.8

* Add a bunch of `Semigroup` instances

## 1.4.7.3

* Remove defunct reference to SpecialResponse [#925](https://github.com/yesodweb/yesod/issues/925)

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
