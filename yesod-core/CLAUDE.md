# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

yesod-core is the core package of the Yesod web framework for Haskell. It provides the fundamental functionality for type-safe, RESTful web applications including routing, dispatch, handlers, widgets, and sessions.

## Build Commands

### Building the library
```bash
stack build yesod-core
```

### Running tests
```bash
# Run all tests
stack test yesod-core

# Run specific test suite
stack test yesod-core:test:test-routes
stack test yesod-core:test:tests
```

### Working with GHC interactively
```bash
stack repl yesod-core
```

## Architecture

### Core Layers

The codebase is organized into several architectural layers:

1. **Routing Layer** (`Yesod.Routes.*`)
   - Route parsing and overlap detection (`Yesod.Routes.Parse`, `Yesod.Routes.Overlap`)
   - Template Haskell code generation for routes (`Yesod.Routes.TH.*`)
   - Type classes for route rendering and parsing (`Yesod.Routes.Class`)

2. **Core Types** (`Yesod.Core.Types`)
   - `HandlerT`: The monad transformer for handling requests
   - `WidgetT`: The monad for composing HTML, CSS, and JavaScript
   - `YesodRequest`: Augmented WAI request with parsed parameters
   - `SessionMap` and `SessionBackend`: Session management abstractions

3. **Handler Layer** (`Yesod.Core.Handler`, `Yesod.Core.Class.Handler`)
   - Request information extraction (parameters, cookies, headers)
   - Response generation (typed content, streaming, redirects)
   - Composable handler actions

4. **Widget System** (`Yesod.Core.Widget`)
   - Modular HTML components with CSS/JS dependencies
   - Unique ID generation for avoiding conflicts
   - Integration with Shakespeare template languages (Hamlet, Cassius, Lucius, Julius)

5. **Dispatch System** (`Yesod.Core.Dispatch`, `Yesod.Core.Class.Dispatch`)
   - Type classes: `YesodDispatch`, `YesodSubDispatch`, `YesodDispatchNested`
   - Template Haskell generation of dispatch code via `mkYesod` and related functions
   - Conversion to WAI applications

6. **Yesod Type Class** (`Yesod.Core.Class.Yesod`)
   - Defines the main `Yesod` type class with customization points
   - Default implementations for error handling, layouts, sessions, CSRF protection
   - Middleware configuration (logging, gzip, method override)

### Template Haskell Code Generation

The framework heavily uses Template Haskell for compile-time route safety. Key TH functions:

- **`mkYesod`**: Generates Route datatype, RenderRoute instance, ParseRoute instance, and YesodDispatch instance from route definitions
- **`mkYesodData`**: Generates only the Route datatype and RenderRoute instance (no dispatch)
- **`mkYesodDispatch`**: Generates only the YesodDispatch instance (assumes Route datatype exists)
- **`mkYesodSubData`/`mkYesodSubDispatch`**: Similar, but for subsites

Route generation options are controlled via `RouteOpts`:
- `setEqDerived`, `setShowDerived`, `setReadDerived`: Control deriving clauses
- `setFocusOnNestedRoute`: Focus code generation on a specific nested route subtree (allows splitting route definitions across modules)
- `setCreateResources`: Control generation of `resourcesSite` value
- `setParameterizedSubroute`: Handle parameterized subroutes
- `setNestedRouteFallthrough`: Control 404 behavior in nested routes (fallthrough vs immediate 404)

### Route Definition Syntax

Routes are typically defined using the `parseRoutes` quasi-quoter:

```haskell
mkYesod "App" [parseRoutes|
/               HomeR    GET
/blog/#BlogId   BlogPostR GET POST
/wiki/*Texts    WikiR
/admin          AdminR:
    /           AdminHomeR GET
    /users      AdminUsersR GET POST
|]
```

This generates:
- A `Route App` datatype with constructors for each route
- `RenderRoute` instance (route to path pieces and query parameters)
- `ParseRoute` instance (path pieces to route)
- `YesodDispatch` instance (WAI request to handler)

### Nested Routes and Subsites

**Nested Routes** (`ResourceParent`): Hierarchical route structure where parent routes share path prefixes. The `setFocusOnNestedRoute` option allows generating code for nested routes in separate modules, with the `YesodDispatchNested` type class enabling dispatch without knowledge of the full route tree.

**Subsites**: Separate applications mounted at a path. Use `Subsite` dispatch type with a function to get the subsite from the master site. Requires `YesodSubDispatch` instance.

### Handler Execution Flow

1. WAI request arrives at `yesodDispatch`
2. Path and method are extracted and matched against generated dispatch code
3. Appropriate handler is selected (or 404/405 returned)
4. Handler runs in `HandlerT` monad with access to:
   - Site foundation value (via `getYesod`)
   - Request information (via `getRequest`, `waiRequest`)
   - Session (via session functions)
   - Database connections and other resources (via type-level caching in `TypeCache`)
5. Handler returns `TypedContent` (or uses specialized response functions)
6. Yesod middleware processes response (CSRF, sessions, etc.)
7. WAI response is returned

### Session Management

Sessions are managed through the `SessionBackend` abstraction:
- `sbLoadSession`: Takes a WAI request, returns session data and a save function
- Default implementation uses client-side encrypted sessions via `clientsession`
- Session cookies can be customized with `customizeSessionCookies`
- Security options: `sslOnlySessions`, `laxSameSiteSessions`, `strictSameSiteSessions`

### Testing Patterns

Test files in `test/` demonstrate:
- Direct use of TH functions for route generation (see `test/RouteSpec.hs`, `test/Hierarchy.hs`)
- Custom dispatcher type class for testing without full Yesod infrastructure
- Integration tests using hspec and wai-test utilities (see `test/YesodCoreTest/`)

The test suite structure:
- `test-routes`: Tests route parsing, rendering, and dispatch logic
- `tests`: Integration tests for handlers, widgets, sessions, CSRF, etc.

### Important Internal Modules

- `Yesod.Core.Internal.TH`: Implementation of `mkYesod` and related TH functions
- `Yesod.Core.Internal.Run`: Core handler execution (`yesodRunner`, `runHandler`)
- `Yesod.Core.Internal.Session`: Session backend implementations
- `Yesod.Routes.TH.Dispatch`: Dispatch clause generation for pattern matching routes
- `Yesod.Routes.TH.RenderRoute`: RenderRoute instance generation
- `Yesod.Routes.TH.ParseRoute`: ParseRoute instance generation

## Current Work (Branch: mattp/nested-route-discovery)

### Completed
- Updated `mkRenderRouteNestedClauses` in `Yesod.Routes.TH.RenderRoute.hs` to render full routes from root instead of just fragments
  - `ResourceLeaf` case now builds complete path including all parent static/dynamic pieces
  - `ResourceParent` case now properly accumulates parent args (combining parent dynamic pieces with current route's dynamic pieces)
- Fixed `prePieces` accumulation in `mkRouteCon` to pass accumulated pieces to children instead of resetting to empty
- Added `mkParentPieces` helper function to build path expressions from parent piece specifications

### Remaining Work

- Redesign the `YesodDispatchNested` class to have the following signature:
  ```
  class (RenderRouteNested route) => YesodDispatchNested route where
      yesodDispatchNested :: ParentArgs route -> YesodRunnerEnv (ParentSite route) -> Maybe W.Application
  ```
- Rework the Template Haskell generation code to work like this. Instead of working on the `[Text]` fragments, generate an `Application` that can respond to the entire WAI Request object.
- This implies that the `YesodDispatchNested` must take a full `Request` path, not a truncated one.

### Test Status


### Recent Implementation (This Session)
1. **Implemented `mkRenderRouteNestedInstanceOpts`** (lines 707-880):
   - Generates datatype for focused nested routes with proper constructors
   - Generates `RenderRouteNested` instance with correct `ParentArgs` and `ParentSite`
   - Handles nested `ResourceParent` children recursively
   - Includes local helper functions `mkRouteConsOpts'` and `mkRouteCon'`

2. **Fixed critical bug** in `mkRenderRouteInstanceOpts` (line 670):
   - Changed from passing `ress` (full resource tree) to `ress'` (focused route's children)
   - This was causing the wrong constructors to be generated

3. **Added TypeFamilies extension** to test files:
   - `test/Hierarchy/Admin.hs`
   - `test/Hierarchy/Nest.hs`
   - `test/Hierarchy/Nest2.hs`
   - `test/Hierarchy/Nest3.hs`
   - `test/Hierarchy/Nest2/NestInner.hs`
   - Required for generated `RenderRouteNested` instances to compile
