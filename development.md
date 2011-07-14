# Scaffolding

## Test suite

Run this from the project root directory. It will make sure each site type builds. It first does an sdist, which ensures we are testing what will be put on hackage.

  shelltest tests/scaffold.shelltest

## Quicker, repeatable site building

Useful for debugging individual failures.

  tests/runscaffold.sh < sqlite-input.txt

## Getting a list of scaffold files for the cabal file

It is necessary after adding a scaffolding file to add it to the list of files in the cabal file.

  find scaffold -type f
