{-# LANGUAGE FlexibleContexts #-}

module Yesod.Form.Option where

import Yesod.Core
import Yesod.Form.Fields

-- | Creates an `OptionList` from a `List`, using the `PathPiece` instance for
-- the external value and a custom function for the user-facing value.
--
-- @since 1.7.7
--
-- PathPiece instances should provide suitable external values, since path
-- pieces serve to be exposed through URLs or HTML anyway. Show/Read instances
-- are avoided here since they could leak internal representations to forms,
-- query params, javascript etc.
--
-- === __Example usage__
--
-- @messages/en.msg
-- > MsgSalesTeam: Sales Team
-- > MsgSalesHead: Head of Sales Team
-- > MsgTechTeam: Tech Team
-- > MsgTechHead: Head of Tech Team
--
-- @example.hs
-- > data UserRole = URSalesTeam | URSalesHead | URTechTeam | URTechHead
-- >
-- > instance PathPiece UserDepartment where
-- >   toPathPiece = \case
-- >     URSalesTeam -> "sales-team"
-- >     URSalesHead -> "sales-head"
-- >     URTechTeam -> "tech-team"
-- >     URTechHead -> "tech-head"
-- >   fromPathPiece = \case
-- >     "sales-team" -> Just URSalesTeam
-- >     "sales-head" -> Just URSalesHead
-- >     "tech-team" -> Just URTechTeam
-- >     "tech-head" -> Just URTechHead
-- >     _ -> Nothing
-- >
-- > userRoleOptions ::
-- >   (MonadHandler m, RenderMessage (HandlerSite m) msg) => m (OptionList UserRole)
-- > userRoleOptions = optionsFromList' userRoles toMsg
-- >   where
-- >   userRoles = [URSalesTeam, URSalesHead, URTechTeam, URTechHead]
-- >   toMsg = \case
-- >     URSalesTeam -> MsgSalesTeam
-- >     URSalesHead -> MsgSalesHead
-- >     URTechTeam -> MsgTechTeam
-- >     URTechHead -> MsgTechHead
--
-- userRoleOptions, will produce an OptionList with the following attributes:
--
-- > +----------------+----------------+--------------------+
-- > | Internal Value | External Value | User-facing Value  |
-- > +----------------+----------------+--------------------+
-- > | URSalesTeam    | sales-team     | Sales Team         |
-- > +----------------+----------------+--------------------+
-- > | URSalesHead    | sales-head     | Head of Sales Team |
-- > +----------------+----------------+--------------------+
-- > | URTechTeam     | tech-team      | Tech Team          |
-- > +----------------+----------------+--------------------+
-- > | URTechHead     | tech-head      | Head of Tech Team  |
-- > +----------------+----------------+--------------------+

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

-- | Creates an `OptionList` from an `Enum`.
--
-- @since 1.7.7
--
-- optionsEnum' == optionsFromList' [minBound..maxBound]
--
-- Creates an `OptionList` containing every constructor of `a`, so that these
-- constructors do not need to be typed out. Bounded and Enum instances must
-- exist for `a` to use this.
optionsEnum' ::
     MonadHandler m
  => RenderMessage (HandlerSite m) msg
  => PathPiece a
  => Enum a
  => Bounded a
  => (a -> msg)
  -> m (OptionList a)
optionsEnum' = optionsFromList' [minBound..maxBound]
