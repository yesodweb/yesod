## yesod-bin: the Yesod executable

This executable is almost exclusively used for its `yesod devel`
capabilities, providing a development server for web apps. It also
provides some legacy functionality, almost all of which has been
superceded by functionality in the
[Haskell Stack build tool](http://haskellstack.org/). This README will
speak exclusively about `yesod devel`.

__CAVEAT__ There may be some issues using `yesod devel` in Docker-enabled
projects. See [comment on
Github](https://github.com/yesodweb/yesod/pull/1305#issuecomment-263204471).

### Development server

The development server will automatically recompile your application
whenever you make source code changes. It will then launch your app,
and reverse-proxy to it. The reverse proxying ensures that you can
connect to your application on a dedicated port, always get the latest
version available, and won't get dropped connections when the app
isn't yet ready. Instead, you'll get some very motivating messages:

![Motivation](https://i.sli.mg/nO6DvN.png)

## Common workflows

The standard Yesod scaffoldings are configured to work with `yesod
devel` out of the box (though see below for non-Yesod
development). For the most part, from within your application
directory, you'll just want to run:

* `stack build yesod-bin`
* `stack exec -- yesod devel`

This will install the corresponding version of the `yesod` executable
into your currently selected snapshot, and then use that
executable. (Starting with version 1.5.0, you can be more lax and use
a `yesod` executable compiled for a different snapshot. Once 1.5.0 is
more widespread we'll probably update these instructions.)

Some other common questions:

* If you want to control which port you can access your application
  on, use the `--port` command line option, e.g. `stack exec -- yesod
  devel --port 4000`. Changing your port inside your source code _will
  not work_, because you need to change the reverse proxying port.
* If you want to run a command after each successful build, you can
  use `stack exec -- yesod devel --success-hook "echo Yay!"`
* If for some reason you want to disable the reverse proxy
  capabilities, use `stack exec -- yesod devel
  --disable-reverse-proxy`

## How it works

The workflow of the devel server is pretty simple:

* Launch a reverse proxy server
* Use Stack file-watch capability to run a build loop on your code,
  rebuilding each time a file is modified
* Have Stack call `yesod devel-signal` to write to a specific file
  (`yesod-devel/rebuild`) each time a rebuild is successful
* Each time `yesod-devel/rebuild` is modified:
  * Kill the current child process
  * Get a new random port
  * Tell the reverse proxy server about the new port to forward to
  * Run the application's devel script with two environment variables
    set:
    * `PORT` gives the newly generated random port. The application
      needs to listen on that port.
    * `DISPLAY_PORT` gives the port that the reverse proxy is
      listening on, used for display purposes or generating URLs.

Now some weird notes:

* The devel script can be one of the following three files. `yesod
  devel` will search for them in the given order. That script must
  provide a `main` function.
  * `app/devel.hs`
  * `devel.hs`
  * `src/devel.hs`
* Unfortunately, directly killing the `ghc` interpreter has never
  worked reliably, so we have an extra hack: when killing the process,
  `yesod devel` also writes to a file
  `yesod-devel/devel-terminate`. Your devel script should respect this
  file and shutdown whenever it exists.
  (It may be fixed in 1.6.0.5.)
* If your .cabal file defines them, `yesod devel` will tell Stack to
  build with the flags `dev` and `library-only`. You can use this to
  speed up compile times (biggest win: skip building executables, thus
  the name `library-only`).

If that all seems a little complicated, remember that the Yesod
scaffolding handles all of this for you. But if you want to implement
it yourself...

## Non-Yesod development

If you'd like to use the `yesod devel` server for your non-Yesod
application, or even for a Yesod application not based on the
scaffolding, this section is for you! We've got a
[sample application in the repository](https://github.com/yesodweb/yesod/tree/master/yesod-bin/devel-example)
that demonstrates how to get this set up. It demonstrates a good way
to jump through the hoops implied above.

One important note: I highly recommend putting _all_ of the logic in
your library, and then providing a `develMain :: IO ()` function which
your `app/devel.hs` script reexports as `main`. I've found this to
greatly simplify things overall, since you can ensure all of your
dependencies are specified correctly in your `.cabal` file. Also, I
recommend using `PackageImports` in that file, as the example app
shows.
