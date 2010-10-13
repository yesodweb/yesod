{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Parse forms (and query strings).
module Yesod.Form
    ( -- * Data types
      GForm
    , FormResult (..)
    , Enctype (..)
    , FormFieldSettings (..)
    , Textarea (..)
      -- * Type synonyms
    , Form
    , Formlet
    , FormField
    , FormletField
    , FormInput
      -- * Unwrapping functions
    , runFormGet
    , runFormPost
    , runFormGet'
    , runFormPost'
      -- * Field/form helpers
    , fieldsToTable
    , fieldsToPlain
    , checkForm
      -- * Template Haskell
    , mkToForm
      -- * Re-exports
    , module Yesod.Form.Fields
    , module Yesod.Form.Class
    ) where

import Yesod.Form.Core
import Yesod.Form.Fields
import Yesod.Form.Class
import Yesod.Form.Profiles (Textarea (..))

import Text.Hamlet
import Yesod.Request
import Yesod.Handler
import Control.Applicative hiding (optional)
import Data.Maybe (fromMaybe, mapMaybe)
import "transformers" Control.Monad.IO.Class
import Control.Monad ((<=<))
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Language.Haskell.TH.Syntax
import Database.Persist.Base (EntityDef (..), PersistEntity (entityDef))
import Data.Char (toUpper, isUpper)
import Yesod.Widget
import Control.Arrow ((&&&))
import Data.List (group, sort)

-- | Display only the actual input widget code, without any decoration.
fieldsToPlain :: FormField sub y a -> Form sub y a
fieldsToPlain = mapFormXml $ mapM_ fiInput

-- | Display the label, tooltip, input code and errors in a single row of a
-- table.
fieldsToTable :: FormField sub y a -> Form sub y a
fieldsToTable = mapFormXml $ mapM_ go
  where
    go fi = do
        wrapWidget (fiInput fi) $ \w -> [$hamlet|
%tr
    %td
        %label!for=$fiIdent.fi$ $fiLabel.fi$
        .tooltip $fiTooltip.fi$
    %td
        ^w^
    $maybe fiErrors.fi err
        %td.errors $err$
|]

runFormGeneric :: Env
               -> FileEnv
               -> GForm sub y xml a
               -> GHandler sub y (FormResult a, xml, Enctype)
runFormGeneric env fe (GForm f) =
    runReaderT (runReaderT (evalStateT f $ IntSingle 1) env) fe

-- | Run a form against POST parameters.
runFormPost :: GForm sub y xml a
            -> GHandler sub y (FormResult a, xml, Enctype)
runFormPost f = do
    rr <- getRequest
    (pp, files) <- liftIO $ reqRequestBody rr
    runFormGeneric pp files f

-- | Run a form against POST parameters, disregarding the resulting HTML and
-- returning an error response on invalid input.
runFormPost' :: GForm sub y xml a -> GHandler sub y a
runFormPost' = helper <=< runFormPost

-- | Run a form against GET parameters, disregarding the resulting HTML and
-- returning an error response on invalid input.
runFormGet' :: GForm sub y xml a -> GHandler sub y a
runFormGet' = helper <=< runFormGet

helper :: (FormResult a, b, c) -> GHandler sub y a
helper (FormSuccess a, _, _) = return a
helper (FormFailure e, _, _) = invalidArgs e
helper (FormMissing, _, _) = invalidArgs ["No input found"]

-- | Run a form against GET parameters.
runFormGet :: GForm sub y xml a
           -> GHandler sub y (FormResult a, xml, Enctype)
runFormGet f = do
    gs <- reqGetParams `fmap` getRequest
    runFormGeneric gs [] f

-- | Create 'ToForm' instances for the given entity. In addition to regular 'EntityDef' attributes understood by persistent, it also understands label= and tooltip=.
mkToForm :: PersistEntity v => v -> Q [Dec]
mkToForm =
    fmap return . derive . entityDef
  where
    afterPeriod s =
        case dropWhile (/= '.') s of
            ('.':t) -> t
            _ -> s
    beforePeriod s =
        case break (== '.') s of
            (t, '.':_) -> Just t
            _ -> Nothing
    getSuperclass (_, _, z) = getTFF' z >>= beforePeriod
    getTFF (_, _, z) = maybe "toFormField" afterPeriod $ getTFF' z
    getTFF' [] = Nothing
    getTFF' (('t':'o':'F':'o':'r':'m':'F':'i':'e':'l':'d':'=':x):_) = Just x
    getTFF' (_:x) = getTFF' x
    getLabel (x, _, z) = fromMaybe (toLabel x) $ getLabel' z
    getLabel' [] = Nothing
    getLabel' (('l':'a':'b':'e':'l':'=':x):_) = Just x
    getLabel' (_:x) = getLabel' x
    getTooltip (_, _, z) = fromMaybe "" $ getTooltip' z
    getTooltip' (('t':'o':'o':'l':'t':'i':'p':'=':x):_) = Just x
    getTooltip' (_:x) = getTooltip' x
    getTooltip' [] = Nothing
    getId (_, _, z) = fromMaybe "" $ getId' z
    getId' (('i':'d':'=':x):_) = Just x
    getId' (_:x) = getId' x
    getId' [] = Nothing
    getName (_, _, z) = fromMaybe "" $ getName' z
    getName' (('n':'a':'m':'e':'=':x):_) = Just x
    getName' (_:x) = getName' x
    getName' [] = Nothing
    derive :: EntityDef -> Q Dec
    derive t = do
        let cols = map ((getId &&& getName) &&& ((getLabel &&& getTooltip) &&& getTFF)) $ entityColumns t
        ap <- [|(<*>)|]
        just <- [|pure|]
        nothing <- [|Nothing|]
        let just' = just `AppE` ConE (mkName $ entityName t)
        string' <- [|string|]
        ftt <- [|fieldsToTable|]
        ffs' <- [|FormFieldSettings|]
        let stm "" = nothing
            stm x = just `AppE` LitE (StringL x)
        let go_ = go ap just' ffs' stm string' ftt
        let c1 = Clause [ ConP (mkName "Nothing") []
                        ]
                        (NormalB $ go_ $ zip cols $ map (const nothing) cols)
                        []
        xs <- mapM (const $ newName "x") cols
        let xs' = map (AppE just . VarE) xs
        let c2 = Clause [ ConP (mkName "Just") [ConP (mkName $ entityName t)
                            $ map VarP xs]]
                        (NormalB $ go_ $ zip cols xs')
                        []
        let y = mkName "y"
        let ctx = map (\x -> ClassP (mkName x) [VarT y])
                $ map head $ group $ sort
                $ mapMaybe getSuperclass
                $ entityColumns t
        return $ InstanceD ctx ( ConT ''ToForm
                              `AppT` ConT (mkName $ entityName t)
                              `AppT` VarT y)
            [FunD (mkName "toForm") [c1, c2]]
    go ap just' ffs' stm string' ftt a =
        let x = foldl (ap' ap) just' $ map (go' ffs' stm string') a
         in ftt `AppE` x
    go' ffs' stm string' (((theId, name), ((label, tooltip), tff)), ex) =
        let label' = string' `AppE` LitE (StringL label)
            tooltip' = string' `AppE` LitE (StringL tooltip)
            ffs = ffs' `AppE`
                  label' `AppE`
                  tooltip' `AppE`
                  (stm theId) `AppE`
                  (stm name)
         in VarE (mkName tff) `AppE` ffs `AppE` ex
    ap' ap x y = InfixE (Just x) ap (Just y)

toLabel :: String -> String
toLabel "" = ""
toLabel (x:rest) = toUpper x : go rest
  where
    go "" = ""
    go (c:cs)
        | isUpper c = ' ' : c : go cs
        | otherwise = c : go cs
