{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module System.CLI.Command.Internal.Types
  ( Command (..)
  ) where

import           System.Console.CmdArgs (Data, Typeable)


-- | Supported interpreter comands.
--
data Command = Cat (Maybe FilePath)                -- ^ read given file
             | Echo [String]                       -- ^ print given strings
             | Wc (Maybe FilePath)                 -- ^ line, word and byte count
             | Pwd                                 -- ^ path to current directory
             | Exit                                -- ^ exit CLI
             | Assignment String String            -- ^ assign environmental variable to given value
             | Grep { grI    :: Bool               -- ^ case sensitivity. True == case insensitive
                    , grW    :: Bool               -- ^ find whole words
                    , grA    :: Int                -- ^ print as many lines after match
                    , grArgs :: [String]           -- ^ arguments of grep command
                    }
             | ExternalCommand String              -- ^ command that is unknown to CLI
  deriving (Eq, Show, Data, Typeable)
