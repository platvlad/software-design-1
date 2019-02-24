module Main where

import           System.CLI.Environment (runCLIMonad)
import           System.CLI.Runner      (runCLI)

main :: IO ()
main = runCLIMonad runCLI >> pure ()
