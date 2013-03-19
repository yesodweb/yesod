{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module YesodCoreTest.NoOverloadedStringsSub where

import Yesod.Core
import Network.Wai
import Yesod.Core.Types

data Subsite = Subsite (forall master. Yesod master => YesodSubRunnerEnv Subsite master (HandlerT master IO) -> Application)

mkYesodSubData "Subsite" [parseRoutes|
/bar BarR GET
/baz BazR GET
/bin BinR GET
/has-one-piece/#Int OnePiecesR GET
/has-two-pieces/#Int/#Int TwoPiecesR GET
/has-three-pieces/#Int/#Int/#Int ThreePiecesR GET
|]

instance Yesod master => YesodSubDispatch Subsite (HandlerT master IO) where
    yesodSubDispatch ysre =
        f ysre
      where
        Subsite f = ysreGetSub ysre $ yreSite $ ysreParentEnv ysre
