module System.CLI.Command.Internal.Wc
  ( wcCommand
  ) where


import           Data.Char              (isSpace)
import           System.CLI.Environment (Stream, fromStream, lengthStream)

-- | Calculate number of lines, words and bytes in given 'Stream'.
--
wcCommand :: Stream -> (Int, Int, Int)
wcCommand stream = (lc, wc, bc)
  where
    string = fromStream stream

    ls = lines string
    lc = length ls

    wc = sum $ fmap (length . filter (not . all isSpace) . flip splitBySpaces []) ls
    bc = lengthStream stream

    splitBySpaces :: String -> String -> [String]
    splitBySpaces [] prev  = [prev]
    splitBySpaces (x : xs) prev | isSpace x = prev : splitBySpaces xs []
                                | otherwise = splitBySpaces xs (prev ++ [x])
