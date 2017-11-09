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

## Coding Guidelines

### Safety

Avoid partial functions. Even if you know the partial function is safe in your instance, partial functions require more reasoning from the programmer and are not resilient to refactoring. For the rare cases where a partial function is appropriate, a custom `error` should be used.

### Style 

Keep coding style consistent with the rest of the file, but don't worry about style too much otherwise. PRs changing code style are viewed skeptically.

### Dependencies

Avoid adding unnecessary dependencies. If a dependency provides only a minor convenience for your implementation, it's probably better to skip it.

If you do add a new dependency, try to support a wide range of versions of it.

### Backwards Compatibility

Backwards incompatible changes are viewed skeptically—best to ask in an issue to see if a particular backwards incompatible change would be approved. If possible keep backwards compatibility by adding new APIs and deprecating old ones.

Keep backwards compatibility with old versions of dependencies when possible.

## PR Guidelines

### PR Scope

As much as possible, keep separate changes in separate PRs.

### Testing

Tests are recommended, but not required.

### Documentation

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

Examples are recommended, but not required, in documentation. Marking new APIs with `@since <version number>` is required.

### Versioning

Yesod packages roughly follow the Haskell Package Versioning Policy style of A.B.C.[D] (MAJOR.MAJOR.MINOR.[PATCH])

* A - Used for massive changes in the library. (Example: 1.2.3.4 becomes 2.0.0)
* B - Used for smaller breaking changes, like removing, renaming, or changing behavior of existing public API. (Example: 1.2.3.4 becomes 1.3.0)
* C - Used for new public APIs (Example: 1.2.3.4 becomes 1.2.4)
* D - Used for bug fixes (Example: 1.2.3.4 becomes 1.2.3.5).
	* D is optional in the version number, so 2.0.0 is a valid version.

Documentation changes don't require a new version.

If you feel there is ambiguity to a change (e.g. fixing a bug in a function, when people may be relying on the old broken behavior), you can ask in an issue or pull request.

Unlike in the Package Versioning Policy, deprecations are not counted as MAJOR changes.

In some cases, dropping compatibility with a major version of a dependency (e.g. changing from transformers >= 0.3 to transformers >= 0.4), is considered a breaking change.

### Changelog

After you submit a PR, update the subproject's Changelog.md file with the new version number and a link to your PR. If your PR does not need to bump the version number, include the change in an "Unreleased" section at the top.

### Releases

Releases should be done as soon as possible after a pull request is merged—don't be shy about reminding us to make a release if we forget.