## 1.6.3

* Add assertFailure
[#1499](https://github.com/yesodweb/yesod/pull/1499)

## 1.6.2

* Add byLabel-related functions like byLabelContain
[#1482](https://github.com/yesodweb/yesod/pull/1482)

## 1.6.1

* Fix the build with `base-4.11` (GHC 8.4).

## 1.6.0

* Upgrade to yesod-core 1.6.0

## 1.5.9.1

* Fixes a Haddock syntax error in 1.5.9 [#1473](https://github.com/yesodweb/yesod/pull/1473)

## 1.5.9
* Add byLabelExact and related functions
[#1459](https://github.com/yesodweb/yesod/pull/1459)

## 1.5.8
* Added implicit parameter HasCallStack to assertions.
[#1421](https://github.com/yesodweb/yesod/pull/1421)

## 1.5.7

* Add clickOn.
[#1408](https://github.com/yesodweb/yesod/pull/1408)

## 1.5.6

* Add assertNotEq.
[#1375](https://github.com/yesodweb/yesod/pull/1375)

## 1.5.5

* Fix warnings

## 1.5.4.1

* Compilation fix for GHC 7.8

## 1.5.4

* yesod-test: add getLocation test helper. [#1314](https://github.com/yesodweb/yesod/pull/1314)

## 1.5.3

* Added bodyNotContains [#1271](https://github.com/yesodweb/yesod/pull/1271)

## 1.5.2

* Added assertEq, deprecated assertEqual [#1259](https://github.com/yesodweb/yesod/pull/1259)

## 1.5.1.1

* Fix `addToken_` needing a trailing space and allows multiples spaces in css selector.

## 1.5.1.0

* Better error provenance for stuff invoking withResponse' [#1191](https://github.com/yesodweb/yesod/pull/1191)

## 1.5.0.1

* Fixed the `application/x-www-form-urlencoded` header being added to all requests, even those sending a binary POST body [#1064](https://github.com/yesodweb/yesod/pull/1064/files)
	* The `application/x-www-form-urlencoded` Content-Type header is now only added if key-value POST parameters are added
	* If no key-values pairs are added, or the request body is set with `setRequestBody`, no default Content-Type header is set

## 1.5

* remove deprecated addNonce functions
* You can now configure testing middleware

Configuring middleware makes it easy to add logging among other things.
middleware is applied to the wai app before each test.

If you follow the yesod scaffold, you probably have a
withApp function in TestImport.hs.
This function should now return (foundation, middleware).
`id` is an acceptable value for middleware.


## 1.4.4

test helpers for CRSF middleware such as addTokenFromCookie

## 1.4.3.2

* Add `addTokenFromCookie` and `addTokenFromCookieNamedToHeaderNamed`, which support the new CSRF token middleware [#1058](https://github.com/yesodweb/yesod/pull/1058)
* Add `getRequestCookies`, which returns the cookies from the most recent request [#1058](https://github.com/yesodweb/yesod/pull/1058)

## 1.4.3.1

* Improved README

## 1.4.2

Provide `Example` instance for `YesodExample`.

## 1.4.1.1

Upgrade to hspec 2
