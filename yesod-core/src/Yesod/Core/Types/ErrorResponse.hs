{-# LANGUAGE DeriveGeneric #-}

module Yesod.Core.Types.ErrorResponse where

import GHC.Generics (Generic)
import Data.Text
import Control.DeepSeq (NFData)
import qualified Network.HTTP.Types as H

-- | Responses to indicate some form of an error occurred.
data ErrorResponse =
      NotFound
        -- ^ The requested resource was not found.
        -- Examples of when this occurs include when an incorrect URL is used, or @yesod-persistent@'s 'get404' doesn't find a value.
        -- HTTP status: 404.
    | InternalError !Text
        -- ^ Some sort of unexpected exception.
        -- If your application uses `throwIO` or `error` to throw an exception, this is the form it would take.
        -- HTTP status: 500.
    | InvalidArgs ![Text]
        -- ^ Indicates some sort of invalid or missing argument, like a missing query parameter or malformed JSON body.
        -- Examples Yesod functions that send this include 'requireCheckJsonBody' and @Yesod.Auth.GoogleEmail2@.
        -- HTTP status: 400.
    | NotAuthenticated
        -- ^ Indicates the user is not logged in.
        -- This is thrown when 'isAuthorized' returns 'AuthenticationRequired'.
        -- HTTP code: 401.
    | PermissionDenied !Text
        -- ^ Indicates the user doesn't have permission to access the requested resource.
        -- This is thrown when 'isAuthorized' returns 'Unauthorized'.
        -- HTTP code: 403.
    | BadMethod !H.Method
        -- ^ Indicates the URL would have been valid if used with a different HTTP method (e.g. a GET was used, but only POST is handled.)
        -- HTTP code: 405.
    deriving (Show, Eq, Generic)

instance NFData ErrorResponse
