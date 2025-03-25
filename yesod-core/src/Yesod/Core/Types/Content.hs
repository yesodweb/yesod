module Yesod.Core.Types.Content where

import qualified Data.ByteString.Builder as BB
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit (Flush, ConduitT)
import Network.Wai (FilePart)

data Content = ContentBuilder !BB.Builder !(Maybe Int) -- ^ The content and optional content length.
             | ContentSource !(ConduitT () (Flush BB.Builder) (ResourceT IO) ())
             | ContentFile !FilePath !(Maybe FilePart)
             | ContentDontEvaluate !Content
