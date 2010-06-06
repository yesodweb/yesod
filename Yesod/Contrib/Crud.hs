{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Yesod.Contrib.Crud where

import Yesod hiding (Form)
import Database.Persist
import Control.Applicative.Error
import Yesod.Contrib.Formable
import Yesod.Contrib.Persist
import Text.Formlets
import Control.Arrow (second)
import Data.Monoid (mempty)

runForm :: Form xml (GHandler sub y) a -> GHandler sub y (Failing a, xml)
runForm f = do
    req <- getRequest
    (pp, _) <- liftIO $ reqRequestBody req
    let env = map (second Left) pp
    let (a, b, _) = runFormState env f
    a' <- a
    return (a', b)

class Formable a => Item a where
    itemTitle :: a -> String

data Crud master item = Crud
    { crudSelect :: GHandler (Crud master item) master [(Key item, item)]
    , crudReplace :: Key item -> item -> GHandler (Crud master item) master ()
    , crudInsert :: item -> GHandler (Crud master item) master (Key item)
    , crudGet :: Key item -> GHandler (Crud master item) master (Maybe item)
    , crudDelete :: Key item -> GHandler (Crud master item) master ()
    }

mkYesodSub "Crud master item"
    [ ("master", [''Yesod])
    , ("item", [''Item])
    , ("Key item", [''SinglePiece])
    ] [$parseRoutes|
/               CrudListR        GET
/add            CrudAddR         GET POST
/edit/#String   CrudEditR        GET POST
/delete/#String CrudDeleteR      GET POST
|]

getCrudListR :: (Yesod master, Item item, SinglePiece (Key item))
             => GHandler (Crud master item) master RepHtml
getCrudListR = do
    items <- getYesodSub >>= crudSelect
    toMaster <- getRouteToMaster
    applyLayout "Items" mempty [$hamlet|
%h1 Items
%ul
    $forall items item
        %li
            %a!href=@toMaster.CrudEditR.toSinglePiece.fst.item@
                $cs.itemTitle.snd.item$
%p
    %a!href=@toMaster.CrudAddR@ Add new item
|]

getCrudAddR :: (Yesod master, Item item, SinglePiece (Key item))
            => GHandler (Crud master item) master RepHtml
getCrudAddR = crudHelper
                "Add new"
                (Nothing :: Maybe (Key item, item))
                False

postCrudAddR :: (Yesod master, Item item, SinglePiece (Key item))
             => GHandler (Crud master item) master RepHtml
postCrudAddR = crudHelper
                "Add new"
                (Nothing :: Maybe (Key item, item))
                True

getCrudEditR :: (Yesod master, Item item, SinglePiece (Key item))
             => String -> GHandler (Crud master item) master RepHtml
getCrudEditR s = do
    itemId <- maybe notFound return $ itemReadId s
    crud <- getYesodSub
    item <- crudGet crud itemId >>= maybe notFound return
    crudHelper
        "Edit item"
        (Just (itemId, item))
        False

postCrudEditR :: (Yesod master, Item item, SinglePiece (Key item))
              => String -> GHandler (Crud master item) master RepHtml
postCrudEditR s = do
    itemId <- maybe notFound return $ itemReadId s
    crud <- getYesodSub
    item <- crudGet crud itemId >>= maybe notFound return
    crudHelper
        "Edit item"
        (Just (itemId, item))
        False

getCrudDeleteR :: (Yesod master, Item item, SinglePiece (Key item))
               => String -> GHandler (Crud master item) master RepHtml
getCrudDeleteR s = do
    itemId <- maybe notFound return $ itemReadId s
    crud <- getYesodSub
    item <- crudGet crud itemId >>= maybe notFound return
    toMaster <- getRouteToMaster
    applyLayout "Confirm delete" mempty [$hamlet|
%form!method=post!action=@toMaster.CrudDeleteR.s@
    %h1 Really delete?
    %p
        %input!type=submit!value=Yes
        \ 
        %a!href=@toMaster.CrudListR@ No
|]

postCrudDeleteR :: (Yesod master, Item item, SinglePiece (Key item))
                => String -> GHandler (Crud master item) master RepHtml
postCrudDeleteR s = do
    itemId <- maybe notFound return $ itemReadId s
    crud <- getYesodSub
    toMaster <- getRouteToMaster
    crudDelete crud itemId
    redirect RedirectTemporary $ toMaster CrudListR

itemReadId :: SinglePiece x => String -> Maybe x
itemReadId = either (const Nothing) Just . fromSinglePiece

crudHelper
    :: (Item a, Yesod master, SinglePiece (Key a))
    => String -> Maybe (Key a, a) -> Bool
    -> GHandler (Crud master a) master RepHtml
crudHelper title me isPost = do
    crud <- getYesodSub
    (errs, form) <- runForm $ formable $ fmap snd me
    toMaster <- getRouteToMaster
    errs' <- case (isPost, errs) of
                (True, Success a) -> do
                    eid <- case me of
                            Just (eid, _) -> do
                                crudReplace crud eid a
                                return eid
                            Nothing -> crudInsert crud a
                    redirect RedirectTemporary $ toMaster $ CrudEditR
                                               $ toSinglePiece eid
                (True, Failure e) -> return $ Just e
                (False, _) -> return Nothing
    applyLayout title mempty [$hamlet|
%p
    %a!href=@toMaster.CrudListR@ Return to list
%h1 $cs.title$
$maybe errs' es
    %ul
        $forall es e
            %li $cs.e$
%form!method=post
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit
                $maybe me e
                    \ 
                    %a!href=@toMaster.CrudDeleteR.toSinglePiece.fst.e@ Delete
|]

defaultCrud :: (Persist i (YesodDB a (GHandler (Crud a i) a)), YesodPersist a)
            => a -> Crud a i
defaultCrud = const $ Crud
    { crudSelect = runDB $ select [] []
    , crudReplace = \a -> runDB . replace a
    , crudInsert = runDB . insert
    , crudGet = runDB . get
    , crudDelete = runDB . delete
    }
