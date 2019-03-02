module System.CLI.Environment.Internal.Types
  ( CLIMonad
  , RuntimeEnv (..)
  , Stream
  , StreamUnit
  , VarState
  , runCLIMonad
  ) where

import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Control.Monad.State.Strict (StateT (..), execStateT)
import           Data.ByteString            (ByteString)
import           Data.Map.Strict            (Map)

-- | We keep runtime variables in a 'Map'.
--
type VarState = Map String String

-- | Stream is represented as 'ByteString'.
--
type Stream = ByteString

-- | StreamUnit is one single element of 'Stream'.
--
type StreamUnit = Char

-- | Environment that we need during interpreter's runtime.
--
data RuntimeEnv = RuntimeEnv { varState :: VarState
                             , stream   :: Stream
                             , exit     :: Bool
                             }
  deriving (Eq, Show)

initEnv :: RuntimeEnv
initEnv = RuntimeEnv mempty mempty False

-- | Monad that is used to perform CLI's computations.
--
type CLIMonad = StateT RuntimeEnv (ExceptT String IO)

-- | Extracts 'IO' computation from 'CLIMonad'.
--
runCLIMonad :: CLIMonad () -> IO (Either String RuntimeEnv)
runCLIMonad cliM = runExceptT resS
  where
    resS = execStateT cliM initEnv

