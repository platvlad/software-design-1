{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.CLI.Command.Internal.Executor
  ( executeCommand
  ) where

import           Control.Exception                           (IOException, try)
import           Control.Monad                               (when)
import           Control.Monad.Except                        (throwError)
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.State.Strict                  (get, modify)
import           System.CLI.Command.Internal.ExternalCommand (externalCommand)
import           System.CLI.Command.Internal.Grep            (grepCommand)
import           System.CLI.Command.Internal.Types           (Command (..))
import           System.CLI.Command.Internal.Wc              (wcCommand)
import           System.CLI.Environment                      (CLIMonad,
                                                              RuntimeEnv (..),
                                                              Stream, insertVar,
                                                              intercalateStream,
                                                              readStream,
                                                              setExit,
                                                              setStream,
                                                              toStream)
import           System.Directory                            (getCurrentDirectory)

-- | Execute given 'Command' according to its semantics.
--
executeCommand :: Command -> CLIMonad ()
executeCommand command = do
    re <- get

    case command of
      Cat (Just path)       -> safeReadFile path >>= modify . setStream
      Cat Nothing           -> pure ()
      Echo args             -> modify $ setStream $ intercalateStream " " (fmap toStream args)
      Wc pathM              -> do
          input <- maybe (pure $ stream re) safeReadFile pathM

          let (lc, wc, bc) = wcCommand input
          let stdout       = intercalateStream " " $ toStream . show <$> [lc, wc, bc]

          modify (setStream stdout)
      Pwd                   -> liftIO (fmap toStream getCurrentDirectory) >>= modify . setStream
      Exit                  -> modify (setExit True) >> modify (setStream "")
      Assignment var val    -> modify (insertVar var val) >> modify (setStream "")
      Grep{..}              -> do
          when (null $ grArgs !! 0) (throwError "grep: empty pattern")

          input  <- if length grArgs == 1 then pure $ stream re else safeReadFile $ grArgs !! 1
          stdout <- either throwError pure $ grepCommand grI grW grA (head grArgs) input

          modify (setStream stdout)
      ExternalCommand shell -> externalCommand shell
  where
    safeReadFile :: String -> CLIMonad Stream
    safeReadFile file = do
      resE <- liftIO $ (try (readStream file) :: IO (Either IOException Stream))
      either (throwError . show) pure resE


