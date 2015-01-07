# Overview

The yesod package groups together the various Yesod related packages into one cohesive whole.

For the yesod executeable, see [yesod-bin](/yesod-bin)

# Scaffolding

## Test suite

test/run.sh

## Getting a list of scaffold files for the cabal file

It is necessary after adding a scaffolding file to add it to the list of files in the cabal file.

  find scaffold -type f
