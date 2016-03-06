## yesod-form

Form handling for Yesod, in the same style as formlets. See [the forms
chapter](http://www.yesodweb.com/book/forms) of the Yesod book.

This package provies a set of basic form inputs such as text, number, time,
checkbox, select, textarea, and etc. via `Yesod.Form.Fields` module.  Also,
there is `Yesod.Form.Nic` module providing richtext field using Nic editor.
However, this module is grandfathered now and Nic editor is not actively
maintained since June 2012.  You can find additional richtext editor fields in
[`yesod-form-richtext`][yesod-form-richtext] package (currently in provides
support of [Summernote][summernote] editor only).

[yesod-form-richtext]:http://hackage.haskell.org/package/yesod-form-richtext
[summernote]:http://summernote.org/
