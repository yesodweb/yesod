{-# LANGUAGE FlexibleContexts #-}

module Yesod.Form.Option where

import Yesod.Core
import Yesod.Form.Fields

-- | Creates an 'OptionList' from a 'List', using the 'PathPiece' instance for
-- the external value and a custom function for the user-facing value.
-- NB: We choose PathPiece as the most suitable class for generating External
-- Values. The motivation is to avoid Show/Read instances which could leak
-- an internal representation to forms, query params, javascript etc.
optionsFromList' ::
     MonadHandler m
  => RenderMessage (HandlerSite m) msg
  => PathPiece a
  => [a]
  -> (a -> msg)
  -> m (OptionList a)
optionsFromList' lst toDisplay = do
  mr <- getMessageRender
  pure $ mkOptionList $ flip map lst $ \v -> Option
    { optionDisplay = mr $ toDisplay v
    , optionInternalValue = v
    , optionExternalValue = toPathPiece v
    }

-- | Creates an 'OptionList' from an 'Enum'
optionsEnum' ::
     MonadHandler m
  => RenderMessage (HandlerSite m) msg
  => PathPiece a
  => Enum a
  => Bounded a
  => (a -> msg)
  -> m (OptionList a)
optionsEnum' = optionsFromList' [minBound..maxBound]
