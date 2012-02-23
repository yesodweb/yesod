{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
#include "../../config.h"
-- | Module for using a kerberos authentication service.
--
-- Please note that all configuration should have been done
-- manually on the machine prior to running the code.
--
-- On linux machines the configuration might be in /etc/krb5.conf.
-- It's worth checking if the Kerberos service provider (e.g. your university)
-- already provide a complete configuration file.
--
-- Be certain that you can manually login from a shell by typing
--
-- > kinit username
--
-- If you fill in your password and the program returns no error code,
-- then your kerberos configuration is setup properly.
-- Only then can this module be of any use.
module Web.Authenticate.Kerberos
  ( loginKerberos
  , KerberosAuthResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Control.Monad (msum, guard)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import System.Exit (ExitCode(..))

-- | Occurreable results of a Kerberos login
data KerberosAuthResult = Ok
                        | NoSuchUser
                        | WrongPassword
                        | TimeOut
                        | UnknownError Text

instance Show KerberosAuthResult where
  show Ok                 = "Login sucessful"
  show NoSuchUser         = "Wrong username"
  show WrongPassword      = "Wrong password"
  show TimeOut            = "kinit respone timeout"
  show (UnknownError msg) = "Unkown error: " ++ T.unpack msg


-- Given the errcode and stderr, return error-value
interpretError :: Int -> Text -> KerberosAuthResult
interpretError _ errmsg = fromJust . msum $
    ["Client not found in Kerberos database while getting" --> NoSuchUser,
     "Preauthentication failed while getting" --> WrongPassword,
     Just $ UnknownError errmsg]
  where
    substr --> kError = guard (substr `T.isInfixOf` errmsg) >> Just kError

-- | Given the username and password, try login to Kerberos service
loginKerberos :: Text -- ^ Username
              -> Text -- ^ Password
              -> IO KerberosAuthResult
loginKerberos username password = do
    timedFetch <- timeout (10*1000000) fetch
    case timedFetch of
      Just res -> return res
      Nothing  -> return TimeOut
  where
    fetch :: IO KerberosAuthResult
    fetch = do
      (exitCode, _out, err) <- readProcessWithExitCode
#ifdef HAVE_HEIMDAL
          "kinit" ["--password-file=STDIN", T.unpack username] (T.unpack password)
#else
          "kinit" [T.unpack username] (T.unpack password)
#endif
      case exitCode of
        ExitSuccess   -> return Ok
        ExitFailure x -> return $ interpretError x (T.pack err)

