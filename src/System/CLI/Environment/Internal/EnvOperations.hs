module System.CLI.Environment.Internal.EnvOperations
  ( insertVar
  , setExit
  , setStream
  ) where

import qualified Data.Map.Strict                       as M (insert)
import           System.CLI.Environment.Internal.Types (RuntimeEnv (..), Stream)

-- | Sets stream's value in 'RuntimeEnv' to given 'Stream'.
--
setStream :: Stream -> RuntimeEnv -> RuntimeEnv
setStream stream' re = re { stream=stream' }

-- | Sets exit's value in 'RuntimeEnv' to given 'Bool'.
--
setExit :: Bool -> RuntimeEnv -> RuntimeEnv
setExit exit' re = re { exit=exit' }

-- | Inserts new variable into 'RuntimeEnv's varState.
--
insertVar :: String -> String -> RuntimeEnv -> RuntimeEnv
insertVar var val re = re { varState=M.insert var val (varState re) }
