[![Build Status](https://dev.azure.com/yesodweb/yesod/_apis/build/status/yesodweb.yesod?branchName=master)](https://dev.azure.com/yesodweb/yesod/_build/latest?definitionId=3&branchName=master)

# Yesod Web Framework

An advanced web framework using the Haskell programming language. Featuring:

  * safety & security guaranteed at compile time
  * developer productivity: tools for all your basic web development needs
  * raw performance
    * fast, compiled code
    * techniques for constant-space memory consumption
  * asynchronous IO
    * this is built in to the Haskell programming language (like Erlang)

Learn more about Yesod on [its main website](http://www.yesodweb.com/). If you
want to get started using Yesod, we strongly recommend the [quick start
guide](http://www.yesodweb.com/page/quickstart), based on [the Haskell build
tool stack](https://github.com/commercialhaskell/stack#readme).

## Hacking on Yesod

Yesod consists mostly of four repositories:

```bash
git clone --recurse-submodules http://github.com/yesodweb/shakespeare
git clone --recurse-submodules http://github.com/yesodweb/persistent
git clone --recurse-submodules http://github.com/yesodweb/wai
git clone --recurse-submodules http://github.com/yesodweb/yesod
```

Each repository can be built with `stack build`.
