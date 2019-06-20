{-# LANGUAGE OverloadedStrings #-}

module System.CLI.Command.Internal.Pipeline
  ( executePipeline
  ) where

import           Control.Monad.State.Strict           (get)
import           System.CLI.Command.Internal.Executor (executeCommand)
import           System.CLI.Command.Internal.Types    (Command (..))
import           System.CLI.Environment               (CLIMonad,
                                                       RuntimeEnv (..))

-- | Execute several 'Command's that make up the pipeline.
--
executePipeline :: [Command] -> CLIMonad ()
executePipeline []       = pure ()
executePipeline (x : xs) = do
    executeCommand x
    re <- get

    if exit re
      then pure ()
      else executePipeline xs
