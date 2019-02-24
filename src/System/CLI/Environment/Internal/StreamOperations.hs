module System.CLI.Environment.Internal.StreamOperations
  ( fromStream
  , intercalateStream
  , isNullStream
  , lengthStream
  , printStream
  , readStream
  , toStream
  ) where

import qualified Data.ByteString                       as BS (intercalate,
                                                              length, null,
                                                              readFile)
import qualified Data.ByteString.Char8                 as BSC8 (pack, putStrLn,
                                                                unpack)
import           System.CLI.Environment.Internal.Types (Stream)

-- | Prints 'Stream' to the stdout.
--
printStream :: Stream -> IO ()
printStream = BSC8.putStrLn

-- | Converts 'String' to 'Stream'.
--
toStream :: String -> Stream
toStream = BSC8.pack

-- | Converts 'Stream' to 'String'.
--
fromStream :: Stream -> String
fromStream = BSC8.unpack

-- | Transforms list of 'Stream's into one inserting given
-- 'Stream' between elements of that list.
--
intercalateStream :: Stream -> [Stream] -> Stream
intercalateStream = BS.intercalate

-- | Checks that given 'Stream' is empty.
--
isNullStream :: Stream -> Bool
isNullStream = BS.null

-- | Reads 'Stream' from file.
--
readStream :: FilePath -> IO Stream
readStream = BS.readFile

-- | Get length of the 'Stream'.
--
lengthStream :: Stream -> Int
lengthStream = BS.length
