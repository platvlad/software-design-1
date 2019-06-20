{-# LANGUAGE OverloadedStrings #-}

-- | Main loop of CLI.
--
module System.CLI.Runner
  ( runCLI
  ) where

import           Control.Monad              (when)
import           Control.Monad.Except       (catchError)
import           Control.Monad.Fix          (fix)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (get, modify)
import           System.CLI.Command         (executePipeline)
import           System.CLI.Environment     (CLIMonad, RuntimeEnv (..),
                                             isNullStream, printStream,
                                             setStream)
import           System.CLI.Parser          (parseCommands)
import           System.CLI.Preprocessor    (preprocessCommands)

-- | Main loop of the CLI.
--
runCLI :: CLIMonad ()
runCLI = fix $ \loop -> do
    nextCommands <- liftIO getLine
    re           <- get

    let preprocessedCommandsE = preprocessCommands re nextCommands

    case preprocessedCommandsE of
      Left  e                    -> liftIO (putStrLn e) >> loop
      Right preprocessedCommands -> do
          let commandsE = parseCommands preprocessedCommands
          case commandsE of
            Left  e        -> liftIO (putStrLn e) >> loop
            Right commands -> do
                executePipeline commands `catchError` (liftIO . putStr)

                re' <- get

                when (not $ isNullStream $ stream re') $ liftIO (printStream $ stream re')

                if exit re'
                  then pure ()
                  else modify (setStream "") >> loop
