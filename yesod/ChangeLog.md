# ChangeLog for yesod

## 1.6.1.2

* Fix compatibility with template-haskell 2.17 [#1730](https://github.com/yesodweb/yesod/pull/1730)

## 1.6.1.1

* Allow yesod-form 1.7

## 1.6.1.0

* `widgetFileReload` and `widgetFileNoReload` now use absolute paths via the new `globFilePackage` Q Exp which can provide absolute templates paths within the project [#1691](https://github.com/yesodweb/yesod/pull/1691)

## 1.6.0.2

* Replace deprecated decodeFile with decodeFileEither. This should have no semantic impact, but silences a deprecation warning. [#1658](https://github.com/yesodweb/yesod/pull/1658)

## 1.6.0.1

* Remove unnecessary deriving of Typeable

## 1.6.0

* Upgrade to yesod-core 1.6

## 1.4.5

* Fix warnings

## 1.4.4

* Reduce dependencies

## 1.4.3.1

*  Handle exceptions while writing a file in `addStaticContentExternal`

## 1.4.3

* Switch to `Data.Yaml.Config`

## 1.4.2

* Do not parse string environment variables into numbers/booleans [#1061](https://github.com/yesodweb/yesod/issues/1061)

## 1.4.1

Provide the `Yesod.Default.Config2` module, for use by newer scaffoldings.
