{-# LANGUAGE RecordWildCards #-}

-- | Module that is responsible for preprocessing of 'String' with commands
-- before it is parsed.
--
module System.CLI.Preprocessor
  ( preprocessCommands
  ) where

import           Data.Char              (isSpace)
import           Data.List              (findIndex)
import qualified Data.Map.Strict        as M ((!?))
import           Data.Maybe             (fromJust, fromMaybe, isNothing)
import           System.CLI.Environment (RuntimeEnv (..))

-- | Substitute all variables in given 'String' using 'VarState' from 'RuntimeEnv'.
--
preprocessCommands :: RuntimeEnv -> String -> Either String String
preprocessCommands RuntimeEnv{..} commands = res
  where
    indsOfVars = varFinder commands False False 0

    res = fmap snd $ foldl foldingFunc (Right (0, commands)) indsOfVars

    varFinder :: String -> Bool -> Bool -> Int -> [Int]
    varFinder [] _ _ _                             = []
    varFinder (x : xs) inQuotes inBadQuotes curInd = res'
      where
        res' | x == '$' && not inBadQuotes  = curInd : varFinder xs inQuotes inBadQuotes (curInd + 1)
             | x == '\"' && not inBadQuotes = varFinder xs (not inQuotes) inBadQuotes (curInd + 1)
             | x == '\'' && not inQuotes    = varFinder xs inQuotes (not inBadQuotes) (curInd + 1)
             | otherwise                    = varFinder xs inQuotes inBadQuotes (curInd + 1)

    replaceVar :: String -> Int -> Int -> Either String (Int, String)
    replaceVar command' ind acc | condA     = Left $ "Name of variable " ++ varName ++ " contains can't contain '$'."
                                | condB     = Left $ "Can't find variable with name " ++ varName ++ " in the environment."
                                | otherwise = Right (newAcc, newString)
      where
        indexOfEnd = fromMaybe (length command' - ind) $ findIndex endVar $ drop ind command'
        delta      = indexOfEnd - 1

        commandDropped = drop (ind + 1) command'

        varName = take delta commandDropped
        condA   = '$' `elem` varName

        varValM = varState M.!? varName
        condB   = isNothing varValM

        varVal    = fromJust varValM
        newAcc    = acc - (delta + 1) + length varVal
        newString = take ind command' ++ varVal ++ drop delta commandDropped

        endVar :: Char -> Bool
        endVar c = isSpace c || c == '|' || c == '\"' || c == '\''

    foldingFunc :: Either String (Int, String) -> Int -> Either String (Int, String)
    foldingFunc l@Left{} _             = l
    foldingFunc (Right (acc, str)) ind = replaceVar str (acc + ind) acc
