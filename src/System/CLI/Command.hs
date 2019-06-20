-- | Module providing type for CLI's commands and function
-- to execute pipeline of such 'Command's.
--
module System.CLI.Command
  ( Command (..)
  , executePipeline
  ) where

import           System.CLI.Command.Internal.Pipeline (executePipeline)
import           System.CLI.Command.Internal.Types    (Command (..))
