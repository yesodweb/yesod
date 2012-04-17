{-# LANGUAGE TemplateHaskell #-}

module Types where

import Options

mkOptCabalDev name = option name (\o -> o
  { optionLongFlags    = ["dev", "use-cabal-dev"]
  , optionShortFlags   = ['d']
  , optionType         = optionTypeBool
  , optionDefault      = "false"
  , optionDescription  = "use cabal-dev to build the package"
  })

mkOptNoApi name = option name (\o -> o
  { optionLongFlags    = ["no-ghc-api"]
  , optionShortFlags   = ['n']
  , optionType         = optionTypeBool
  , optionDefault      = "false"
  , optionDescription  = "do not use the GHC API to build, use `cabal build' instead"
  })

mkOptApi name = option name (\o -> o
  { optionLongFlags    = ["ghc-api"]
  , optionShortFlags   = ['a']
  , optionType         = optionTypeBool
  , optionDefault      = "false"
  , optionDescription  = "use the GHC API to build (faster, but experimental)"
  })

mkOptSuccessHook name = option name (\o -> o
  { optionLongFlags    = ["success-hook"]
  , optionShortFlags   = ['s']
  , optionType         = optionTypeMaybe optionTypeString
  , optionDefault      = ""
  , optionDescription  = "Shell command to run when compilation succeeds (e.g. 'beep')"
  })

mkOptFailHook name = option name (\o -> o
  { optionLongFlags    = ["fail-hook"]
  , optionShortFlags   = ['f']
  , optionType         = optionTypeMaybe optionTypeString
  , optionDefault      = ""
  , optionDescription  = "Shell command to run when compilation fails (e.g. 'beep')"
  })

mkOptVerbose name = option name (\o -> o
  { optionLongFlags    = ["verbose"]
  , optionShortFlags   = ['v']
  , optionType         = optionTypeBool
  , optionDefault      = "false"
  , optionDescription  = "more verbose output"
  })

