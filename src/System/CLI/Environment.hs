-- | Module that provides types and functions to interact with CLI's
-- state during its runtime.
--
module System.CLI.Environment
  ( CLIMonad
  , RuntimeEnv (..)
  , Stream
  , VarState
  , fromStream
  , insertVar
  , intercalateStream
  , isNullStream
  , lengthStream
  , printStream
  , readStream
  , runCLIMonad
  , setExit
  , setStream
  , splitStream
  , toStream
  ) where

import           System.CLI.Environment.Internal.EnvOperations    (insertVar,
                                                                   setExit,
                                                                   setStream)
import           System.CLI.Environment.Internal.StreamOperations (fromStream, intercalateStream,
                                                                   isNullStream,
                                                                   lengthStream,
                                                                   printStream,
                                                                   readStream,
                                                                   splitStream,
                                                                   toStream)
import           System.CLI.Environment.Internal.Types            (CLIMonad, RuntimeEnv (..),
                                                                   Stream,
                                                                   VarState,
                                                                   runCLIMonad)
