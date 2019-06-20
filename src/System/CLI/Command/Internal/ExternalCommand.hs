module System.CLI.Command.Internal.ExternalCommand
  ( externalCommand
  ) where

import           Control.Exception          (IOException, try)
import           Control.Monad.Except       (throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (get, modify)
import           System.CLI.Environment     (CLIMonad, RuntimeEnv (..),
                                             fromStream, setStream, toStream)
import           System.Exit                (ExitCode (..))
import           System.Process             (readCreateProcessWithExitCode,
                                             shell)

-- | Function that executes 'ExternalCommand' (command that is
-- unknown to the CLI).
--
externalCommand :: String -> CLIMonad ()
externalCommand command = do
    let process = shell command

    re <- get
    let streamS       = fromStream $ stream re
    let readProcessIO = readCreateProcessWithExitCode process streamS

    resE <- liftIO (try readProcessIO :: IO (Either IOException (ExitCode, String, String)))

    case resE of
      Left e                         -> throwError $ show e
      Right (ExitSuccess, stdout, _) -> modify $ setStream $ toStream stdout
      Right (_, _, stderr)           -> throwError stderr
