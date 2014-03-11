{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Helper functions for creating forms when using Bootstrap v3.
module Yesod.Form.Bootstrap3
  ( -- * Rendering forms
    renderBootstrap3
  , BootstrapFormLayout(..)
  , BootstrapGridOptions(..)
    -- * Field settings
  , bfs
  , withPlaceholder
  , withAutofocus
  , withLargeInput
  , withSmallInput
    -- * Submit button
  , bootstrapSubmit
  , mbootstrapSubmit
  , BootstrapSubmit(..)
  ) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.Text (Text)
import Data.String (IsString(..))
import Yesod.Core

import Yesod.Form.Types
import Yesod.Form.Functions

-- | Create a new 'FieldSettings' with the classes that are
-- required by Bootstrap v3.
bfs :: RenderMessage site msg => msg -> FieldSettings site
bfs msg =
    FieldSettings (SomeMessage msg) Nothing Nothing Nothing [("class", "form-control")]


-- | Add a placeholder attribute to a field.  If you need i18n
-- for the placeholder, currently you\'ll need to do a hack and
-- use 'getMessageRender' manually.
withPlaceholder :: Text -> FieldSettings site -> FieldSettings site
withPlaceholder placeholder fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("placeholder", placeholder) : fsAttrs fs


-- | Add an autofocus attribute to a field.
withAutofocus :: FieldSettings site -> FieldSettings site
withAutofocus fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("autofocus", "autofocus") : fsAttrs fs


-- | Add the @input-lg@ CSS class to a field.
withLargeInput :: FieldSettings site -> FieldSettings site
withLargeInput fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("class", " input-lg ") : fsAttrs fs


-- | Add the @input-sm@ CSS class to a field.
withSmallInput :: FieldSettings site -> FieldSettings site
withSmallInput fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("class", " input-sm ") : fsAttrs fs


-- | How many bootstrap grid columns should be taken (see
-- 'BootstrapFormLayout').
data BootstrapGridOptions =
    ColXs !Int
  | ColSm !Int
  | ColMd !Int
  | ColLg !Int
    deriving (Eq, Ord, Show)

toColumn :: BootstrapGridOptions -> String
toColumn (ColXs 0) = ""
toColumn (ColSm 0) = ""
toColumn (ColMd 0) = ""
toColumn (ColLg 0) = ""
toColumn (ColXs columns) = "col-xs-" ++ show columns
toColumn (ColSm columns) = "col-sm-" ++ show columns
toColumn (ColMd columns) = "col-md-" ++ show columns
toColumn (ColLg columns) = "col-lg-" ++ show columns

toOffset :: BootstrapGridOptions -> String
toOffset (ColXs 0) = ""
toOffset (ColSm 0) = ""
toOffset (ColMd 0) = ""
toOffset (ColLg 0) = ""
toOffset (ColXs columns) = "col-xs-offset-" ++ show columns
toOffset (ColSm columns) = "col-sm-offset-" ++ show columns
toOffset (ColMd columns) = "col-md-offset-" ++ show columns
toOffset (ColLg columns) = "col-lg-offset-" ++ show columns

addGO :: BootstrapGridOptions -> BootstrapGridOptions -> BootstrapGridOptions
addGO (ColXs a) (ColXs b) = ColXs (a+b)
addGO (ColSm a) (ColSm b) = ColSm (a+b)
addGO (ColMd a) (ColMd b) = ColMd (a+b)
addGO (ColLg a) (ColLg b) = ColLg (a+b)
addGO a b     | a > b = addGO b a
addGO (ColXs a) other = addGO (ColSm a) other
addGO (ColSm a) other = addGO (ColMd a) other
addGO (ColMd a) other = addGO (ColLg a) other
addGO (ColLg _) _     = error "Yesod.Form.Bootstrap.addGO: never here"


-- | The layout used for the bootstrap form.
data BootstrapFormLayout =
    BootstrapBasicForm
  | BootstrapInlineForm
  | BootstrapHorizontalForm
      { bflLabelOffset :: !BootstrapGridOptions
      , bflLabelSize   :: !BootstrapGridOptions
      , bflInputOffset :: !BootstrapGridOptions
      , bflInputSize   :: !BootstrapGridOptions
      }
    deriving (Show)


-- | Render the given form using Bootstrap v3 conventions.
--
-- Sample Hamlet for 'BootstrapHorizontalForm':
--
-- >  <form .form-horizontal role=form method=post action=@{ActionR} enctype=#{formEnctype}>
-- >    ^{formWidget}
-- >    ^{bootstrapSubmit MsgSubmit}
renderBootstrap3 :: Monad m => BootstrapFormLayout -> FormRender m a
renderBootstrap3 formLayout aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
        widget = [whamlet|
            $newline never
            #{fragment}
            ^{formFailureWidget res}
            $forall view <- views
              <div .form-group :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
                $case formLayout
                  $of BootstrapBasicForm
                    $if fvId view /= bootstrapSubmitId
                      <label for=#{fvId view}>#{fvLabel view}
                    ^{fvInput view}
                    ^{helpWidget view}
                  $of BootstrapInlineForm
                    $if fvId view /= bootstrapSubmitId
                      <label .sr-only for=#{fvId view}>#{fvLabel view}
                    ^{fvInput view}
                    ^{helpWidget view}
                  $of BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
                    $if fvId view /= bootstrapSubmitId
                      <label .control-label .#{toOffset labelOffset} .#{toColumn labelSize} for=#{fvId view}>#{fvLabel view}
                      <div .#{toOffset inputOffset} .#{toColumn inputSize}>
                        ^{fvInput view}
                        ^{helpWidget view}
                    $else
                      <div .#{toOffset (addGO inputOffset (addGO labelOffset labelSize))} .#{toColumn inputSize}>
                        ^{fvInput view}
                        ^{helpWidget view}
                |]
    return (res, widget)


-- | (Internal) Render form failures via alerts.
formFailureWidget :: FormResult a -> WidgetT site IO ()
formFailureWidget (FormFailure reasons) =
    [whamlet|
        $forall reason <- reasons
            <div .alert .alert-error>#{reason}
    |]
formFailureWidget _ = return ()


-- | (Internal) Render a help widget for tooltips and errors.
helpWidget :: FieldView site -> WidgetT site IO ()
helpWidget view = [whamlet|
    $maybe tt <- fvTooltip view
      <span .help-block>#{tt}
    $maybe err <- fvErrors view
      <span .help-block>#{err}
|]


-- | How the 'bootstrapSubmit' button should be rendered.
data BootstrapSubmit msg =
    BootstrapSubmit
        { bsValue   :: msg
          -- ^ The text of the submit button.
        , bsClasses :: Text
          -- ^ Classes added to the @<button>@.
        , bsAttrs   :: [(Text, Text)]
          -- ^ Attributes added to the @<button>@.
        } deriving (Show)

instance IsString msg => IsString (BootstrapSubmit msg) where
    fromString msg = BootstrapSubmit (fromString msg) " btn-default " []


-- | A Bootstrap v3 submit button disguised as a field for
-- convenience.  For example, if your form currently is:
--
-- > Person <$> areq textField "Name"    Nothing
-- >        <*> areq textField "Surname" Nothing
--
-- Then just change it to:
--
-- > Person <$> areq textField "Name"    Nothing
-- >        <*> areq textField "Surname" Nothing
-- >        <*  bootstrapSubmit "Register"
--
-- (Note that @<*@ is not a typo.)
--
-- Alternatively, you may also just create the submit button
-- manually as well in order to have more control over its
-- layout.
bootstrapSubmit
    :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m)
    => BootstrapSubmit msg -> AForm m ()
bootstrapSubmit = formToAForm . liftM (second return) . mbootstrapSubmit


-- | Same as 'bootstrapSubmit' but for monadic forms.  This isn't
-- as useful since you're not going to use 'renderBootstrap3'
-- anyway.
mbootstrapSubmit
    :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m)
    => BootstrapSubmit msg -> MForm m (FormResult (), FieldView site)
mbootstrapSubmit (BootstrapSubmit msg classes attrs) =
    let res = FormSuccess ()
        widget = [whamlet|<button class="btn #{classes}" type=submit *{attrs}>_{msg}|]
        fv  = FieldView { fvLabel    = ""
                        , fvTooltip  = Nothing
                        , fvId       = bootstrapSubmitId
                        , fvInput    = widget
                        , fvErrors   = Nothing
                        , fvRequired = False }
    in return (res, fv)


-- | A royal hack.  Magic id used to identify whether a field
-- should have no label.  A valid HTML4 id which is probably not
-- going to clash with any other id should someone use
-- 'bootstrapSubmit' outside 'renderBootstrap3'.
bootstrapSubmitId :: Text
bootstrapSubmitId = "b:ootstrap___unique__:::::::::::::::::submit-id"
