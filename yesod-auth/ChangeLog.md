## 1.6.4

* Make `registerHelper` configurable [#1524](https://github.com/yesodweb/yesod/issues/1524)

## 1.6.3

* Generalize GoogleEmail2.getPerson [#1501](https://github.com/yesodweb/yesod/pull/1501)

## 1.6.2

* Remove MINIMAL praggma for authHttpManager [#1489](https://github.com/yesodweb/yesod/issues/1489)

## 1.6.1

* Relax a number of type signatures [#1488](https://github.com/yesodweb/yesod/issues/1488)

## 1.6.0

* Upgrade to yesod-core 1.6.0

## 1.4.21

* Add redirectToCurrent to Yesod.Auth module for controlling setUltDestCurrent in redirectLogin [#1461](https://github.com/yesodweb/yesod/pull/1461)

## 1.4.20

* Extend `YesodAuthEmail` to support extensible password hashing via
  `hashAndSaltPassword` and `verifyPassword` functions

## 1.4.19

* Adjust English localization to distinguish between "log in" (verb) and "login" (noun)

## 1.4.18

* Expose Yesod.Auth.Util.PasswordStore

## 1.4.17.3

* Some translation fixes

## 1.4.17.2

* Move to cryptonite from cryptohash

## 1.4.17.1

* Some translation fixes

## 1.4.17

* Add Show instance for user credentials `Creds`
* Export pid type for identifying plugin
* Fix warnings
* Allow for a custom Email Login DOM with `emailLoginHandler`

## 1.4.16

* Fix email provider [#1330](https://github.com/yesodweb/yesod/issues/1330)
* Document JSON endpoints of Yesod.Auth.Email

## 1.4.15

* Add JSON endpoints to Yesod.Auth.Email module
* Export croatianMessage from Message module
* Minor Haddock rendering fixes at Auth.Email module

## 1.4.14

* Remove Google OpenID link [#1309](https://github.com/yesodweb/yesod/pull/1309)
* Add CSRF Security check in `registerHelperFunction` [#1302](https://github.com/yesodweb/yesod/pull/1302)

## 1.4.13.5

* Translation fix

## 1.4.13.4

* Improved translations
* peristent 2.6

## 1.4.13.3

* Doc update (and a warning)

## 1.4.13.1

* Add CSRF token to login form from `Yesod.Auth.Dummy` [#1205](https://github.com/yesodweb/yesod/pull/1205)

## 1.4.13

* Add a CSRF token to the login form from `Yesod.Auth.Hardcoded`, making it compatible with the CSRF middleware [#1161](https://github.com/yesodweb/yesod/pull/1161)
* Multiple session messages. [#1187](https://github.com/yesodweb/yesod/pull/1187)

## 1.4.12

* Deprecated Yesod.Auth.GoogleEmail

## 1.4.11

Add Yesod.Auth.Hardcoded

## 1.4.9

* Expose defaultLoginHandler

## 1.4.8

* GoogleEmail2: proper error message when permission denied

## 1.4.7

* add a runHttpRequest function for handling HTTP errors

## 1.4.6

* Use nonce package to generate verification keys and CSRF tokens [#1011](https://github.com/yesodweb/yesod/pull/1011)

## 1.4.5

* Adds export of email verify route [#980](https://github.com/yesodweb/yesod/pull/980)

## 1.4.4

* Add AuthenticationResult and authenticate function [#959](https://github.com/yesodweb/yesod/pull/959)

## 1.4.3

* Added means to fetch user's Google profile [#936](https://github.com/yesodweb/yesod/pull/936)

## 1.4.2

* Perform `onLogout` before session cleaning [#923](https://github.com/yesodweb/yesod/pull/923)

## 1.4.1.3

[Updated french translation of Yesod.Auth.Message. #904](https://github.com/yesodweb/yesod/pull/904)

## 1.4.1

Dutch translation added.
