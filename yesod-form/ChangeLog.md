# ChangeLog for yesod-form

## 1.7.9.1

* Set `base >= 4.11` for less CPP and imports [#1876](https://github.com/yesodweb/yesod/pull/1876)

## 1.7.9

* Added `checkboxesField'` for creating checkbox in more correct way than original `checkboxesField`
  Function `checkboxesField` marked as deprecated. [#1843](https://github.com/yesodweb/yesod/pull/1843)

## 1.7.8

* Added `radioField'` for creating radio button in more correct way than original `radioField`.
  Function `radioField` marked as deprecated. [#1842](https://github.com/yesodweb/yesod/pull/1842)

## 1.7.7

* Added `optionsFromList'` to create an OptionList from a List, using the PathPiece instance for the external value and
a custom function for the user-facing value. Also added `optionsEnum'` to create an OptionList from an enumeration
[#1828](https://github.com/yesodweb/yesod/pull/1828)

## 1.7.6

* Added `datetimeLocalField` for creating a html `<input type="datetime-local">` [#1817](https://github.com/yesodweb/yesod/pull/1817)

## 1.7.5

* Add Romanian translation [#1801](https://github.com/yesodweb/yesod/pull/1801)

## 1.7.4

* Added a `Monad AForm` instance only when `transformers` >= 0.6 [#1795](https://github.com/yesodweb/yesod/pull/1795)

## 1.7.3

* Fixed `radioField` according to Bootstrap 3 docs. [#1783](https://github.com/yesodweb/yesod/pull/1783)

## 1.7.2

* Added `withRadioField` and re-express `radioField` into that. [#1775](https://github.com/yesodweb/yesod/pull/1775)

## 1.7.1

* Added `colorField` for creating a html color field (`<input type="color">`) [#1748](https://github.com/yesodweb/yesod/pull/1748)

## 1.7.0

* Extended `OptionList` by `OptionListGrouped` and implemented grouped select fields (`<select>` with `<optgroup>`) [#1722](https://github.com/yesodweb/yesod/pull/1722)

## 1.6.7

* Added equivalent version of `mreqMsg` for `areq` and `wreq` correspondingly [#1628](https://github.com/yesodweb/yesod/pull/1628)

## 1.6.6

* Added `mreqMsg` for `mreq` functionality with a configurable MsgValueRequired [#1613](https://github.com/yesodweb/yesod/pull/1613)

## 1.6.5

* Add `.sr-only` to labels in `renderBootstrap3` when they are null.

## 1.6.4

* Make FormResult an instance of Eq

## 1.6.3

* make sure a select field does not lose the selected value even if a validation on the
  field fails

## 1.6.2

* Move `addClass` from private/undocumented in `Yesod.Form.Bootstrap3` to `Yesod.Form.Functions` [#1510](https://github.com/yesodweb/yesod/pull/1510)
* Add `Yesod.Form.Functions.removeClass` [#1510](https://github.com/yesodweb/yesod/pull/1510)
* Changed `Textarea` to derive `IsString` [#1514](https://github.com/yesodweb/yesod/pull/1514)
* Expose `selectFieldHelper` [#1530](https://github.com/yesodweb/yesod/pull/1530)

## 1.6.1

* Explicitly define `(<>)` in the `Semigroup` instance for `Enctype`

## 1.6.0

* Upgrade to yesod-core 1.6.0

## 1.4.16

* Korean translation

## 1.4.15

* Added `Alternative` instance to `FormResult` to simplify handling pages with multiple forms.

## 1.4.14

* Added `WForm` to reduce the verbosity using monadic forms.
* Added `wreq` and `wopt` correspondent functions for `WForm`.

## 1.4.13

* Fixed `textareaField` `writeHtmlEscapedChar` trim "\r"

## 1.4.12

* Password field does not remember its previous value

## 1.4.11

* Fix warnings
* Fixed spelling errors and wording for `Yesod.Form.Functions.convertField`'s
  documentation

## 1.4.10

* Fixed `identifyForm` to properly return `FormMissing` for empty forms. [#1072](https://github.com/yesodweb/yesod/issues/1072)

## 1.4.9

* Added a `ToValue` instance for `Enctype` [#1296](https://github.com/yesodweb/yesod/pull/1296)

## 1.4.8

* Added Yesod.Form.I18n.Spanish

## 1.4.6

* Functor instances for Option/OptionList

## 1.4.5

* Foldable/Traversable instances for FormResult [#1089](https://github.com/yesodweb/yesod/pull/1089)

## 1.4.4.1

* runFormPost has wrong behavior for empty forms [#950](https://github.com/yesodweb/yesod/issues/950)

## 1.4.4

* Add a `Semigroup` instance

## 1.4.3

Added `jqueryDatePickerDayField`.

## 1.4.2.1

Documentation updates

## 1.4.2

Added `timeFieldTypeTime` and `timeFieldTypeText`, and deprecated `timeField`
itself.
