{-# LANGUAGE OverloadedStrings #-}

module System.CLI.Command.Internal.Grep
  ( grepCommand
  ) where


import           Data.Bifunctor          (second)
import           Data.Maybe              (fromJust, isNothing)
import           System.CLI.Environment  (Stream, intercalateStream,
                                          splitStream)
import           Text.RE.TDFA.ByteString (SimpleREOptions (..),
                                          compileRegexWith, matches, (*=~))

-- | Grep command that searches matches of pattern in given 'Stream'.
-- isI - case insensitivity flag. isW - match only words flag.
-- nLines - how many lines should be output after each match.
--
grepCommand :: Bool -> Bool -> Int -> String -> Stream -> Either String Stream
grepCommand isI isW nLines pat stream | badRe      = Left "grep: can't parse regular expression"
                                      | otherwise  = Right res
  where
    newPat = if isW then "\\b" ++ pat ++ "\\b" else pat
    option = if isI then BlockInsensitive else BlockSensitive

    reM   = compileRegexWith option newPat
    badRe = isNothing reM

    re = fromJust reM

    linesOfStream  = splitStream '\n' stream
    linesToMatches = zip [0..] $ fmap (*=~ re) linesOfStream

    linesWithMatches = filter ((/= 0) . snd) $ fmap (second (length . matches)) linesToMatches

    res = intercalateStream separator $ fmap processMatch linesWithMatches

    separator = if nLines == 0 then "\n" else "\n--\n" -- grep 12 /Users/alexkane/min.pdb -A 2

    processMatch :: (Int, Int) -> Stream
    processMatch (indOfLine, len) = intercalateStream separator matchesWithApps
      where
        matches' = replicate len $ linesOfStream !! indOfLine

        linesToAppend   = take nLines $ drop (indOfLine + 1) linesOfStream
        matchesWithApps = fmap (intercalateStream "\n" . (: linesToAppend)) matches'
