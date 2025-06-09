# ChangeLog for yesod-static

## 1.6.1.2

* Set `base >= 4.11` for less CPP and imports [#1876](https://github.com/yesodweb/yesod/pull/1876)

## 1.6.1.1

* Use crypton instead of cryptonite [#1838](https://github.com/yesodweb/yesod/pull/1838)

## 1.6.1.0

* Support reproducible embedded file order [#1684](https://github.com/yesodweb/yesod/issues/1684#issuecomment-652562514)

## 1.6.0.2

* Remove unnecessary deriving of Typeable

## 1.6.0.1

* Make test suite build with GHC 8.6 [#1561](https://github.com/yesodweb/yesod/pull/1561)

## 1.6.0

* Upgrade to yesod-core 1.6.0

## 1.5.3.1

* Switch to cryptonite

## 1.5.3

* Add `staticFilesMap` function
* Add `staticFilesMergeMap` function

## 1.5.2

* Fix test case for CRLF line endings
* Fix warnings

## 1.5.1.1

* Fix test suite compilation

## 1.5.1

* yesod-static doesn't obey Authentication [#1286](https://github.com/yesodweb/yesod/issues/1286)

## 1.5.0.5

* Avoid lazy I/O in mkEmbeddedStatic (fixes [#149](https://github.com/yesodweb/yesod/issues/149))

## 1.5.0.4

* Doc tweaks

## 1.5.0

* Drop system-filepath

## 1.4.0.3

Fix bug when `StaticRoute` constructor is not imported.
