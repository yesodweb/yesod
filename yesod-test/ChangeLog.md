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
