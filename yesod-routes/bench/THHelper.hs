{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module THHelper where

import Language.Haskell.TH.Syntax
import Data.Char (toLower)
import Yesod.Routes.TH
import Yesod.Routes.Parse

settings = MkDispatchSettings
    [|\w x y z -> (w, x, y, z)|]
    [|undefined|]
    [|fst|]
    [|\x (_, y) -> (x, y)|]
    [|snd|]
    [|Nothing|]
    [|Nothing|]
    (\(Just method) name -> return $ VarE $ mkName $ map toLower method ++ name)

resources = [parseRoutes|
/ HomeR GET
/foo FooR GET
/bar/#Int BarR GET
/baz BazR GET
|]
