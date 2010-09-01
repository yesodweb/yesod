### Yesod 0.5.0 (August 29, 2010)

* Forms no longer have special types for special views; instead, there is a
toFormField attribute when declaring entities to specify a form rendering
function.

* URL settings for jQuery and Nic are now in their own typeclasses. This will
be the approach used in the future when adding more widgets and forms that
require Javascript libraries.

* You can explicitly specify the id and name attributes to be used in forms if
you like. When omitted, a unique name is automatically generated.

* The isAuthorized function now takes a function specifying whether the
request is a write request. This should make it simpler to develop read/write
authorization systems. Bonus points: if you use HTTP request methods properly,
the isWriteRequest function will automatically determine whether a request is
a read or write request.

* You can now specify splitPath and joinPath functions yourself. Previously,
the built-in versions had very specific URL rules, such as enforcing a
trailing slash. If you want something more flexible, you can override these
functions.

* addStaticContent is used to serve CSS and Javascript code from widgets from
external files. This allows caching to take place as you'd normally like.

* Static files served from the static subsite can have a hash string added to
the query string; this is done automatically when using the getStaticFiles
function. This allows you to set your expires headers far in the future.

* A new Yesod.Mail module provides datatypes and functions for creating
multipart MIME email messages and sending them via the sendmail executable.
Since these functions generate lazy bytestrings, you can use any delivery
mechanism you want.

* Change the type of defaultLayout to use Widgets instead of PageContent. This
makes it easier to avoid double-including scripts and stylesheets.

* Major reworking of the Auth subsite to make it easier to use.

* Update of the site scaffolder to include much more functionality. Also
removed the Handler type alias from the library, as the scaffolder now
provides that.

### New in Yesod 0.4.0

A big thanks on this release to Simon Michael, who pointed out a number of
places where the docs were unclear, the API was unintuitive, or the names were
inconsistent.

* Widgets. These allow you to create composable pieces of a webpage that
keep track of their own Javascript and CSS. It includes a function for
obtaining unique identifiers to avoid name collisions, and does automatic
dependency combining; in other words, if you have two widgets that depend on
jQuery, the combined widget will only include it once.

* Combined the Yesod.Form and Yesod.Formable module into a single, consistent,
widget-based API. It includes basic input functions as well as fancier
Javascript-driven functions; for example, there is a plain day entry field,
and a day entry field which automatically loads the jQuery UI date picker.

* Added the yesod executable which performs basic scaffolding.

* Cleaned up a bunch of API function names for consistency. For example,
Yesod.Request now has a logical lookupGetName, lookupPostName, etc naming
scheme.

* Changed the type of basicHandler to require less typing, and added
basicHandler' which allows you to modify the line output to STDOUT (or skip it
altogether).

* Switched the Handler monad from ContT to MEitherT (provided by the neither
package). ContT does not have a valid MonadCatchIO instance, which is used for
the sqlite persitent backend.

* Facebook support in the Auth helper.

* Ensure that HTTP request methods are given in ALL CAPS.

* Cleaned up signatures of many methods in the Yesod typeclass. In particular,
due to changes in web-routes-quasi, many of those functions can now live in
the Handler monad, making it easier to use standard functions on them.

* The static file helper now has extensible file-extension-to-mimetype
mappings.

* Added the sendResponse function for handler short-circuiting.

* Renamed Routes to Route.
