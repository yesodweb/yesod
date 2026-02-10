module Yesod.Core.Types.Content where

import qualified Data.ByteString.Builder as BB
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit (Flush, ConduitT)
import Network.Wai (FilePart)

data Content
    = ContentBuilder !BB.Builder !(Maybe Int)
    -- ^ The content and optional content length.
    --
    -- Note that, despite @Builder@'s laziness, this is entirely forced
    -- into memory by default in order to catch imprecise exceptions
    -- before beginning to respond. If you are confident you don't have
    -- imprecise exceptions, you may disable this by wrapping the
    -- `ToContent` data in `DontFullyEvaluate`.-
    | ContentSource !(ConduitT () (Flush BB.Builder) (ResourceT IO) ())
    | ContentFile !FilePath !(Maybe FilePart)
    | ContentDontEvaluate !Content
    -- ^ Used internally to wrap @ContentBuilder@s to disable forcing
    -- them. No effect on other @Content@.
