# Contributing

Thanks for your interest in contributing to Yesod! This file has some tips for developing Yesod and getting a pull request accepted.

## Development

Yesod is a mega-repo that contains many Haskell packages, each in a different directory. All the subprojects can be developed with Stack, using `stack <command> <subproject>`, e.g.

* `stack build yesod-form`
* `stack test yesod-auth`
* `stack haddock yesod-websockets`

If you'd like to test your changes in a full-fledged Yesod app, you can use Stack to build against it, e.g.:

```
packages:
- '/path/to/this/repo/yesod-auth'
```

## Testing

Tests are appreciated, but not required, for changes to Yesod.

## Documentation

All public APIs must be documented. Documenting private functions is optional, but may be nice depending on their complexity. Example documentation:

```
-- | Looks up the hidden input named "_token" and adds its value to the params.
--
-- ==== __Examples__
--
-- > request $ do
-- >   addToken_ "#formID"
--
-- @since 1.5.4
addToken_ :: Query -- ^ CSS selector that resolves to the @<form>@ containing the token.
		  -> RequestBuilder site ()
```

Examples are appreciated, but not required, in documentation. Marking new APIs with `@since <version number>` is required.

## Versioning

Yesod packages roughly follow the Haskell Package Versioning Policy, MAJOR.MAJOR.MINOR.PATCH

* MAJOR - Used for massive changes in the library
* MAJOR - Used for smaller breaking changes, like removing functions or changed behavior of a function.
* MINOR - Used for new public APIs
* PATCH - Used for bug fixes and documentationc changes.

If you feel there is ambiguity to a change (e.g. fixing a bug in a function, when people may be relying on the old broken behavior), you can ask in an issue or pull request.

Unlike in the Package Versioning Policy, deprecations are not counted as MAJOR changes.

## Changelog

After you submit a PR, update the subproject's Changelog.md file with a link to your PR
