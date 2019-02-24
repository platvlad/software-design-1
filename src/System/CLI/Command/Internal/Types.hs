module System.CLI.Command.Internal.Types
  ( Command (..)
  ) where

-- | Supported interpreter comands.
--
data Command = Cat (Maybe FilePath)     -- ^ read given file
             | Echo [String]            -- ^ print given strings
             | Wc (Maybe FilePath)      -- ^ line, word and byte count
             | Pwd                      -- ^ path to current directory
             | Exit                     -- ^ exit CLI
             | Assignment String String -- ^ assign environmental variable to given value
             | ExternalCommand String   -- ^ command that is unknown to CLI
  deriving (Eq, Show)
