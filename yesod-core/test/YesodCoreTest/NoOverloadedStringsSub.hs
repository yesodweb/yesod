{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-} -- hah, the test should be renamed...
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- Not actually a problem, we're now requiring overloaded strings, we just need
-- to make the docs more explicit about it.
module YesodCoreTest.NoOverloadedStringsSub where

import Yesod.Core
import Yesod.Core.Types

data Subsite = Subsite (forall master. Yesod master => YesodSubRunnerEnv Subsite master -> Application)

mkYesodSubData "Subsite" [parseRoutes|
/bar BarR GET
/baz BazR GET
/bin BinR GET
/has-one-piece/#Int OnePiecesR GET
/has-two-pieces/#Int/#Int TwoPiecesR GET
/has-three-pieces/#Int/#Int/#Int ThreePiecesR GET
|]

instance Yesod master => YesodSubDispatch Subsite master where
    yesodSubDispatch ysre =
        f ysre
      where
        Subsite f = ysreGetSub ysre $ yreSite $ ysreParentEnv ysre
