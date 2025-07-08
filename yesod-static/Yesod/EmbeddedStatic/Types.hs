{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Yesod.EmbeddedStatic.Types(
    Location
  , Generator
  -- ** Entry
  , Entry
  , ebHaskellName
  , ebLocation
  , ebMimeType
  , ebProductionContent
  , ebDevelReload
  , ebDevelExtraFiles
) where

import Data.Default
import Language.Haskell.TH
import Network.Mime (MimeType)
import qualified Data.ByteString.Lazy as BL

-- | A location is a relative path within the static subsite at which resource(s) are made available.
-- The location can include slashes to simulate directories but must not start or end with a slash.
type Location = String

-- | A single resource embedded into the executable at compile time.
--
-- This data type is a settings type.  For more information, see
-- <http://www.yesodweb.com/book/settings-types>.
data Entry = Entry {
    ebHaskellName :: Maybe Name
        -- ^ An optional haskell name. If the name is present, a variable
        --   of type @Route 'Yesod.EmbeddedStatic.EmbeddedStatic'@ with the
        --   given name will be created which points to this resource.
  , ebLocation :: Location     -- ^ The location to serve the resource from.
  , ebMimeType :: MimeType     -- ^ The mime type of the resource.
  , ebProductionContent :: IO BL.ByteString
        -- ^ If the development argument to 'Yesod.EmbeddedStatic.mkEmbeddedStatic' is False,
        -- then at compile time this action will be executed to load the content.
        -- During development, this action will not be executed.
  , ebDevelReload :: ExpQ
        -- ^ This must be a template haskell expression of type @IO 'BL.ByteString'@.
        -- If the development argument to 'Yesod.EmbeddedStatic.mkEmbeddedStatic' is True,
        -- this action is executed on every request to compute the content.  Most of the
        -- time, 'ebProductionContent' and 'ebDevelReload' should be the same action but
        -- occasionally you might want additional processing inside the 'ebProductionContent'
        -- function like javascript/css minification to only happen when building for production.
  , ebDevelExtraFiles :: Maybe ExpQ
        -- ^ Occasionally, during development an entry needs extra files/resources available
        --   that are not present during production (for example, image files that are embedded
        --   into the CSS at production but left unembedded during development).  If present,
        --   @ebDevelExtraFiles@ must be a template haskell expression of type
        --   @['T.Text'] -> IO (Maybe ('MimeType', 'BL.ByteString'))@.  That is, a function
        --   taking as input the list of path pieces and optionally returning a mime type
        --   and content.
}

-- | When using 'def', you must fill in at least 'ebLocation'.
instance Default Entry where
    def = Entry { ebHaskellName = Nothing
                , ebLocation = "xxxx"
                , ebMimeType = "application/octet-stream"
                , ebProductionContent = return BL.empty
                , ebDevelReload = [| return BL.empty |]
                , ebDevelExtraFiles = Nothing
                }

-- | An embedded generator is executed at compile time to produce the entries to embed.
type Generator = Q [Entry]
