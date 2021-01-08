# Changelog

## 1.7.0.1

[#1716](https://github.com/yesodweb/yesod/pull/1716)

* Fixed bug where duplicating `<option>` tags caused the `value` field to be cleared

## 1.7.0

[#1707](https://github.com/yesodweb/yesod/pull/1707)

* Added delete buttons
* Added support for custom text or icons inside add/delete buttons
* Added new presets for Bootstrap + Font Awesome icons
* Added support for more complex fields that have multiple parts stuch as radio fields
* Improved support for fields that rely on hidden inputs like WYSIWYG editors
* Fixed redundant class in existing Bootstrap presets
* Fixed styling not applying to error messages on individual fields
* Tooltips now show once at the top of the multi-field group when using `amulti`

## 1.6.0

[#1601](https://github.com/yesodweb/yesod/pull/1601)

* Added `Yesod.Form.MultiInput` which supports multi-input forms without needing to submit the form to add an input field