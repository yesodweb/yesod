## 1.6.6

* `defaultErrorHandler` handles text/plain requests [#1522](https://github.com/yesodweb/yesod/pull/1520)

## 1.6.5

* Add `fileSourceByteString` [#1503](https://github.com/yesodweb/yesod/pull/1503)

## 1.6.4

* Add `addContentDispositionFileName` [#1504](https://github.com/yesodweb/yesod/pull/1504)

## 1.6.3

* Add missing export for `SubHandlerFor`

## 1.6.2

* Derive a `Show` instance for `ResourceTree` and `FlatResource` [#1492](https://github.com/yesodweb/yesod/pull/1492)
	* Some third party packages, like `yesod-routes-flow` derive their own `Show` instance, and this will break those packages.

## 1.6.1

* Add a `Semigroup LiteApp` instance, and explicitly define `(<>)` in the
  already existing `Semigroup` instances for `WidgetFor`, `Head`, `Body`,
  `GWData`, and `UniqueList`.

## 1.6.0

* Upgrade to conduit 1.3.0
* Switch to `MonadUnliftIO`
* Drop `mwc-random` and `blaze-builder` dependencies
* Strictify some internal data structures
* Add `CI` wrapper to first field in `Header` data constructor
  [#1418](https://github.com/yesodweb/yesod/issues/1418)
* Internal only change, users of stable API are unaffected: `WidgetT`
  holds its data in an `IORef` so that it is isomorphic to `ReaderT`,
  avoiding state-loss issues..
* Overhaul of `HandlerT`/`WidgetT` to no longer be transformers.
* Fix Haddock comment & simplify implementation for `contentTypeTypes` [#1476](https://github.com/yesodweb/yesod/issues/1476)

## 1.4.37.3

* Improve error message when request body is too large [#1477](https://github.com/yesodweb/yesod/pull/1477)

## 1.4.37.2

* Improve error messages for the CSRF checking functions [#1455](https://github.com/yesodweb/yesod/issues/1455)

## 1.4.37.1

* Fix documentation on `languages` function, update `getMessageRender` to use said function. [#1457](https://github.com/yesodweb/yesod/pull/1457)

## 1.4.37

* Add `setWeakEtag` function in Yesod.Core.Handler module.

## 1.4.36

* Add `replaceOrAddHeader` function in Yesod.Core.Handler module. [1416](https://github.com/yesodweb/yesod/issues/1416)

## 1.4.35.1

* TH fix for GHC 8.2

## 1.4.35

* Contexts can be included in generated TH instances. [1365](https://github.com/yesodweb/yesod/issues/1365)
* Type variables can be included in routes.

## 1.4.34

* Add `WaiSubsiteWithAuth`. [#1394](https://github.com/yesodweb/yesod/pull/1394)

## 1.4.33

* Adds curly brackets to route parser. [#1363](https://github.com/yesodweb/yesod/pull/1363)

## 1.4.32

* Fix warnings
* Route parsing handles CRLF line endings
* Add 'getPostParams' in Yesod.Core.Handler
* Haddock rendering improved.

## 1.4.31

* Add `parseCheckJsonBody` and `requireCheckJsonBody`

## 1.4.30

* Add `defaultMessageWidget`

## 1.4.29

* Exports some internals and fix version bounds [#1318](https://github.com/yesodweb/yesod/pull/1318)

## 1.4.28

* Add ToWidget instances for strict text, lazy text, and text builder [#1310](https://github.com/yesodweb/yesod/pull/1310)

## 1.4.27

* Added `jsAttributes` [#1308](https://github.com/yesodweb/yesod/pull/1308)

## 1.4.26

* Modify `languages` so that, if you previously called `setLanguage`, the newly
  set language will be reflected.

## 1.4.25

* Add instance of MonadHandler and MonadWidget for ExceptT [#1278](https://github.com/yesodweb/yesod/pull/1278)

## 1.4.24

* cached and cachedBy will not overwrite global state changes [#1268](https://github.com/yesodweb/yesod/pull/1268)

## 1.4.23.1

* Don't allow sending multiple cookies with the same name to the client, in accordance with [RFC 6265](https://tools.ietf.org/html/rfc6265). This fixes an issue where multiple CSRF tokens were sent to the client. [#1258](https://github.com/yesodweb/yesod/pull/1258)
* Default CSRF tokens to the root path "/", fixing an issue where multiple tokens were stored in cookies, and using the wrong one led to CSRF errors [#1248](https://github.com/yesodweb/yesod/pull/1248)

## 1.4.23

* urlParamRenderOverride method for Yesod class [#1257](https://github.com/yesodweb/yesod/pull/1257)
* Add laxSameSiteSessions and strictSameSiteSessions [#1226](https://github.com/yesodweb/yesod/pull/1226)

## 1.4.22

* Proper handling of impure exceptions within `HandlerError` values

## 1.4.21

* Add support for `Encoding` from `aeson-0.11` [#1241](https://github.com/yesodweb/yesod/pull/1241)

## 1.4.20.2

* GHC 8 support

## 1.4.20.1

* Log a warning when a CSRF error occurs [#1200](https://github.com/yesodweb/yesod/pull/1200)

## 1.4.20

* `addMessage`, `addMessageI`, and `getMessages` functions

## 1.4.19.1

* Allow lines of dashes in route files [#1182](https://github.com/yesodweb/yesod/pull/1182)

## 1.4.19

* Auth logout not working with defaultCsrfMiddleware [#1151](https://github.com/yesodweb/yesod/issues/1151)

## 1.4.18.2

* Allow subsites within hierarchical routes [#1144](https://github.com/yesodweb/yesod/pull/1144)

## 1.4.18

* Add hook to apply arbitrary function to all handlers [#1122](https://github.com/yesodweb/yesod/pull/1122)

## 1.4.17

* Add `getApprootText`

## 1.4.16

* Add `guessApproot` and `guessApprootOr`

## 1.4.15.1

* bugfix neverExpires leaked threads

## 1.4.15

* mkYesod avoids using reify when it isn't necessary. This avoids needing to define the site type below the call to mkYesod.

## 1.4.14

* Add CSRF protection functions and middleware based on HTTP cookies and headers [#1017](https://github.com/yesodweb/yesod/pull/1017)
* Add mkYesodWith, which allows creating sites with polymorphic type parameters [#1055](https://github.com/yesodweb/yesod/pull/1055)
* Do not define the site type below a call to mkYesod (or any variant), as it will be required at splicing time for reification.
  This was allowed before because reification was not in use. Reification was introduced to allow parametrized types to be used
  by mkYesod (and variants), with potentially polymorphic variables.

## 1.4.13

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
