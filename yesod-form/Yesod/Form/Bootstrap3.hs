{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Helper functions for creating forms when using <http://getbootstrap.com/ Bootstrap 3>.
--

module Yesod.Form.Bootstrap3
  ( -- * Example: Rendering a basic form
    -- $example

    -- * Example: Rendering a horizontal form
    -- $example2

    -- * Rendering forms
    renderBootstrap3
  , BootstrapFormLayout(..)
  , BootstrapGridOptions(..)
    -- * Field settings
    -- $fieldSettings
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
import qualified Text.Blaze.Internal as Blaze
import Yesod.Core
import Yesod.Form.Types
import Yesod.Form.Functions

-- | Create a new 'FieldSettings' with the @form-control@ class that is
-- required by Bootstrap v3.
--
-- Since: yesod-form 1.3.8
bfs :: RenderMessage site msg => msg -> FieldSettings site
bfs msg =
    FieldSettings (SomeMessage msg) Nothing Nothing Nothing [("class", "form-control")]


-- | Add a placeholder attribute to a field.  If you need i18n
-- for the placeholder, currently you\'ll need to do a hack and
-- use 'getMessageRender' manually.
--
-- Since: yesod-form 1.3.8
withPlaceholder :: Text -> FieldSettings site -> FieldSettings site
withPlaceholder placeholder fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("placeholder", placeholder) : fsAttrs fs


-- | Add an autofocus attribute to a field.
--
-- Since: yesod-form 1.3.8
withAutofocus :: FieldSettings site -> FieldSettings site
withAutofocus fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("autofocus", "autofocus") : fsAttrs fs


-- | Add the @input-lg@ CSS class to a field.
--
-- Since: yesod-form 1.3.8
withLargeInput :: FieldSettings site -> FieldSettings site
withLargeInput fs = fs { fsAttrs = newAttrs }
    where newAttrs = addClass "input-lg" (fsAttrs fs)


-- | Add the @input-sm@ CSS class to a field.
--
-- Since: yesod-form 1.3.8
withSmallInput :: FieldSettings site -> FieldSettings site
withSmallInput fs = fs { fsAttrs = newAttrs }
    where newAttrs = addClass "input-sm" (fsAttrs fs)


-- | How many bootstrap grid columns should be taken (see
-- 'BootstrapFormLayout').
--
-- Since: yesod-form 1.3.8
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
--
-- Since: yesod-form 1.3.8
data BootstrapFormLayout =
    BootstrapBasicForm -- ^ A form with labels and inputs listed vertically. See <http://getbootstrap.com/css/#forms-example>
  | BootstrapInlineForm -- ^ A form whose @\<inputs>@ are laid out horizontally (displayed as @inline-block@). For this layout, @\<label>@s are still added to the HTML, but are hidden from display. When using this layout, you must add the @form-inline@ class to your form tag. See <http://getbootstrap.com/css/#forms-inline>
  | BootstrapHorizontalForm
      { bflLabelOffset :: !BootstrapGridOptions -- ^ The left <http://getbootstrap.com/css/#grid-offsetting offset> of the @\<label>@.
      , bflLabelSize   :: !BootstrapGridOptions -- ^ The number of grid columns the @\<label>@ should use.
      , bflInputOffset :: !BootstrapGridOptions -- ^ The left <http://getbootstrap.com/css/#grid-offsetting offset> of the @\<input>@ from its @\<label>@.
      , bflInputSize   :: !BootstrapGridOptions -- ^ The number of grid columns the @\<input>@ should use.
      } -- ^ A form laid out using the Bootstrap grid, with labels in the left column and inputs on the right. When using this layout, you must add the @form-horizontal@ class to your form tag. Bootstrap requires additional markup for the submit button for horizontal forms; you can use 'bootstrapSubmit' in your form or write the markup manually. See <http://getbootstrap.com/css/#forms-horizontal>
    deriving (Show)


-- | Render the given form using Bootstrap v3 conventions.
--
-- Since: yesod-form 1.3.8
renderBootstrap3 :: Monad m => BootstrapFormLayout -> FormRender m a
renderBootstrap3 formLayout aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
        widget = [whamlet|
            $newline never
            #{fragment}
            $forall view <- views
              <div .form-group :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.has-error>
                $case formLayout
                  $of BootstrapBasicForm
                    $if fvId view /= bootstrapSubmitId
                      <label :Blaze.null (fvLabel view):.sr-only for=#{fvId view}>#{fvLabel view}
                    ^{fvInput view}
                    ^{helpWidget view}
                  $of BootstrapInlineForm
                    $if fvId view /= bootstrapSubmitId
                      <label .sr-only for=#{fvId view}>#{fvLabel view}
                    ^{fvInput view}
                    ^{helpWidget view}
                  $of BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
                    $if fvId view /= bootstrapSubmitId
                      <label :Blaze.null (fvLabel view):.sr-only .control-label .#{toOffset labelOffset} .#{toColumn labelSize} for=#{fvId view}>#{fvLabel view}
                      <div .#{toOffset inputOffset} .#{toColumn inputSize}>
                        ^{fvInput view}
                        ^{helpWidget view}
                    $else
                      <div .#{toOffset (addGO inputOffset (addGO labelOffset labelSize))} .#{toColumn inputSize}>
                        ^{fvInput view}
                        ^{helpWidget view}
                |]
    return (res, widget)


-- | (Internal) Render a help widget for tooltips and errors.
helpWidget :: FieldView site -> WidgetFor site ()
helpWidget view = [whamlet|
    $maybe tt <- fvTooltip view
      <span .help-block>#{tt}
    $maybe err <- fvErrors view
      <span .help-block .error-block>#{err}
|]


-- | How the 'bootstrapSubmit' button should be rendered.
--
-- Since: yesod-form 1.3.8
data BootstrapSubmit msg =
    BootstrapSubmit
        { bsValue   :: msg
          -- ^ The text of the submit button.
        , bsClasses :: Text
          -- ^ Classes added to the @\<button>@.
        , bsAttrs   :: [(Text, Text)]
          -- ^ Attributes added to the @\<button>@.
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
-- >        <*  bootstrapSubmit ("Register" :: BootstrapSubmit Text)
--
-- (Note that '<*' is not a typo.)
--
-- Alternatively, you may also just create the submit button
-- manually as well in order to have more control over its
-- layout.
--
-- Since: yesod-form 1.3.8
bootstrapSubmit
    :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m)
    => BootstrapSubmit msg -> AForm m ()
bootstrapSubmit = formToAForm . liftM (second return) . mbootstrapSubmit


-- | Same as 'bootstrapSubmit' but for monadic forms.  This isn't
-- as useful since you're not going to use 'renderBootstrap3'
-- anyway.
--
-- Since: yesod-form 1.3.8
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

-- $example
-- @\<input\>@ tags in Bootstrap 3 require the @form-control@ class,
-- and so they need modified 'FieldSettings' to display correctly.
--
-- When creating your forms, use the 'bfs' function to add this class:
--
-- > personForm :: AForm Handler Person
-- > personForm = Person
-- >        <$> areq textField (bfs ("Name" :: Text)) Nothing
-- >        <*> areq textField (bfs ("Surname" :: Text)) Nothing
--
-- That form can then be rendered into a widget using the 'renderBootstrap3' function. Here, the form is laid out vertically using 'BootstrapBasicForm':
--
-- > (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm personForm
--
-- And then used in Hamlet:
--
-- >  <form role=form method=post action=@{ActionR} enctype=#{formEnctype}>
-- >    ^{formWidget}
-- >    <button type="submit" .btn .btn-default>Submit

-- $example2
-- Yesod.Form.Bootstrap3 also supports <http://getbootstrap.com/css/#forms-horizontal horizontal, grid based forms>.
-- These forms require additional markup for the submit tag, which is provided by the 'bootstrapSubmit' function:
--
-- > personForm :: AForm Handler Person
-- > personForm = Person
-- >        <$> areq textField MsgName Nothing
-- >        <*> areq textField MsgSurname Nothing
-- >        <*  bootstrapSubmit (BootstrapSubmit MsgSubmit "btn-default" [("attribute-name","attribute-value")])
-- >        -- Note: bootstrapSubmit works with all BootstrapFormLayouts, but provides the additional markup required for Bootstrap's horizontal forms.
--
-- That form can be rendered with specific grid spacing:
--
-- > (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) personForm
--
-- And then used in Hamlet. Note the additional @form-horizontal@ class on the form, and that a manual submit tag isn't required:
--
-- >  <form .form-horizontal role=form method=post action=@{ActionR} enctype=#{formEnctype}>
-- >    ^{formWidget}

-- $fieldSettings
-- This module comes with several methods to help customize your Bootstrap 3 @\<input\>@s.
-- These functions can be chained together to apply several properties to an input:
--
-- > userForm :: AForm Handler UserForm
-- > userForm = UserForm
-- >        <$> areq textField nameSettings Nothing
-- >      where nameSettings = withAutofocus $
-- >                           withPlaceholder "First name" $
-- >                           (bfs ("Name" :: Text))
