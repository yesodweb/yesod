# Overview

The yesod package

* groups together the various Yesod related packages into one cohesive whole.
* contains the executable yesod
  * yesod devel - start a development environment
  * yesod init - create a scaffolded site

# Scaffolding

## Test suite

test/run.sh

## Getting a list of scaffold files for the cabal file

It is necessary after adding a scaffolding file to add it to the list of files in the cabal file.

  find scaffold -type f
