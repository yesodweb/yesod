{-# LANGUAGE
    TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses
  , OverloadedStrings, StandaloneDeriving, FlexibleInstances, FlexibleContexts
  , ViewPatterns, UndecidableInstances, ConstraintKinds
  #-}


module YesodCoreTest.ParameterizedSite.SubRoute where

import Yesod.Core
import Data.Kind (Type)

type Constraints p = (Eq (Key p), Show (Key p), Read (Key p), PathPiece (Key p), Show p)

class Constraints p => SiteClass p where
  type Key p :: Type

newtype SubRoute a v = SubRoute a
  deriving (Eq, Show, Read)

instance SiteClass Int where
  type Key Int = Int

mkYesodOpts (setParameterizedSubroute True defaultOpts) "(SiteClass p) => SubRoute p v" [parseRoutes|
/home/#{Key p} HomeR GET
/editor EditorR:
  /away/#{Key p} AwayR GET
|]

{-
The above generates data structures and instances like the following:

  data Route (SubRoute p v) = HomeR (Key p) | EditorR (EditorR p v)

deriving instance SiteClass p => Eq (Route (SubRoute p v))
deriving instance SiteClass p => Show (Route (SubRoute p v))
deriving instance SiteClass p => Read (Route (SubRoute p v))
data EditorR p v = AwayR (Key p)
deriving instance SiteClass p => Eq (EditorR p v)
deriving instance SiteClass p => Show (EditorR p v)
deriving instance SiteClass p => Read (EditorR p v)

Note that `p` is now threaded through the other data structures.

Otherwise, EditorR's definition would've been:
data EditorR
  = AwayR (Key p)
  deriving (Eq, Show, Read)
which clearly doesn't work, as `p` is not in scope.
-}

-- | Opt-in guard (the counterpart to the backwards-compat guard in
-- "YesodCoreTest.ParameterizedSubDispatch"). With 'setParameterizedSubroute'
-- enabled, the nested subroute datatype /does/ carry the parent's type
-- parameters — here 'EditorR' has kind @Type -> Type -> Type@ — so the
-- threaded constraints stay well-scoped. This signature only compiles if the
-- opt-in still parameterizes the subroute, proving the nested-discovery
-- feature remains available behind the flag.
_editorRParamGuard :: EditorR p v -> EditorR p v
_editorRParamGuard = id

instance SiteClass a => Yesod (SubRoute a v)

getHomeR :: SiteClass a => Key a -> HandlerFor (SubRoute a v) Html
getHomeR key = do
    SubRoute x <- liftHandler getYesod
    defaultLayout
        [whamlet|
            <p>
                Stub #{show x} #{show key}
        |]

getAwayR :: SiteClass a => Key a -> HandlerFor (SubRoute a v) Html
getAwayR key1 = do
    SubRoute x <- liftHandler getYesod
    defaultLayout
        [whamlet|
            <p>
                Stub #{show x} #{show key1}
        |]
