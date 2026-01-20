# ChangeLog for yesod-auth

## 1.6.12.0

* Add `afterPasswordRouteHandler` [#1863](https://github.com/yesodweb/yesod/pull/1863)
* Use crypton instead of cryptonite [#1838](https://github.com/yesodweb/yesod/pull/1838)
* Set `base >= 4.11` for less CPP and imports [#1876](https://github.com/yesodweb/yesod/pull/1876)


## 1.6.11.3

* Add Romanian translation [#1809](https://github.com/yesodweb/yesod/pull/1809)

## 1.6.11.2

* Add support for aeson 2.2 [#1820](https://github.com/yesodweb/yesod/pull/1820)

## 1.6.11.1

* No star is type [#1797](https://github.com/yesodweb/yesod/pull/1797)

## 1.6.11

* Add support for aeson 2

## 1.6.10.5

* Fix German translations of AuthMessage [#1741](https://github.com/yesodweb/yesod/pull/1741)

## 1.6.10.4

* Add support for GHC 9 [#1737](https://github.com/yesodweb/yesod/pull/1737)

## 1.6.10.3

* Relax bounds for yesod-form 1.7

## 1.6.10.2

* Relax bounds for persistent 2.12

## 1.6.10.1

* Add support for Persistent 2.11 [#1701](https://github.com/yesodweb/yesod/pull/1701)

## 1.6.10

* Updated `AuthMessage` data type in `Yesod.Auth.Message` to accommodate registration flow where password is supplied initially: deprecated `AddressVerified` and split into `EmailVerifiedChangePass` and `EmailVerified`
* Fixed a bug in `getVerifyR` related to the above, where the incorrect message was displayed when password was set during registration
* Added `sendForgotPasswordEmail` to `YesodAuthEmail` typeclass, allowing for different emails for account registration vs. forgot password
* See pull request [#1662](https://github.com/yesodweb/yesod/pull/1662)

## 1.6.9

* Added `registerHelper` and `passwordResetHelper` methods to the `YesodAuthEmail` class, allowing for customizing behavior for user registration and forgot password requests [#1660](https://github.com/yesodweb/yesod/pull/1660)
* Exposed `defaultRegisterHelper` as default implementation for the above methods

## 1.6.8.1

* Email: Fix typo in `defaultEmailLoginHandler` template [#1605](https://github.com/yesodweb/yesod/pull/1605)
* Remove unnecessary deriving of Typeable

## 1.6.8

* Dummy: Add support for JSON submissions [#1619](https://github.com/yesodweb/yesod/pull/1619)

## 1.6.7

* Redirect behavior of `clearCreds` depends on request type [#1598](https://github.com/yesodweb/yesod/pull/1598)

## 1.6.6

* Deprecated `Yesod.Auth.GoogleEmail2`, see [#1579](https://github.com/yesodweb/yesod/issues/1579) and [migration blog post](https://pbrisbin.com/posts/googleemail2_deprecation/)

## 1.6.5

* Add support for persistent 2.9 [#1516](https://github.com/yesodweb/yesod/pull/1516), [#1561](https://github.com/yesodweb/yesod/pull/1561)

## 1.6.4.1

* Email: Fix forgot-password endpoint [#1537](https://github.com/yesodweb/yesod/pull/1537)

## 1.6.4

* Make `registerHelper` configurable [#1524](https://github.com/yesodweb/yesod/issues/1524)
* Email: Immediately register with a password [#1389](https://github.com/yesodweb/yesod/issues/1389)
To configure this new functionality:
  1. Define `addUnverifiedWithPass`, e.g:
  ```
  addUnverified email verkey = liftHandler $ runDB $ do
    void $ insert $ UserLogin email Nothing (Just verkey) False
    return email

  addUnverifiedWithPass email verkey pass = liftHandler $ runDB $ do
    void $ insert $ UserLogin email (Just pass) (Just verkey) False
    return email
  ```
  2. Add a `password` field to your client forms.

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
