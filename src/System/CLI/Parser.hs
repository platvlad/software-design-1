{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Module that is used to parse 'Command's.
--
module System.CLI.Parser
  ( parseCommands
  ) where

import           Control.Applicative   (many, some, (<|>))
import           Control.Monad         (when)
import           Data.Bifunctor        (first)
import           Data.List             (findIndices, isPrefixOf)
import           Data.Maybe            (listToMaybe)
import           System.CLI.Command    (Command (..))
import           Text.Megaparsec       (Parsec, eof, getInput, lookAhead, parse,
                                        takeWhile1P, try)
import           Text.Megaparsec.Char  (alphaNumChar, char, printChar, space,
                                        space1, string)
import           Text.Megaparsec.Error (ShowErrorComponent (..),
                                        errorBundlePretty)

-- | Alias for 'Parsec' monad.
--
type Parser = Parsec String String

-- | Delimiter between commands in pipeline.
--
delimiter :: Char
delimiter = '|'

-- | Parse given 'String' into number of 'Command's.
--
parseCommands :: String -> Either String [Command]
parseCommands = first errorBundlePretty . parse commandsP ""

-- | This instance is needed to use 'errorBundlePretty'.
--
instance ShowErrorComponent String where
  showErrorComponent = id

commandsP :: Parser [Command]
commandsP = do
    res <- many commandP

    stream <- getInput

    if null stream
      then pure res
      else fail "Can't parse whole command."

commandP :: Parser Command
commandP = assignmentP <|> catP <|> wcP <|> pwdP <|> echoP <|> exitP <|> externalP

externalP :: Parser Command
externalP = wrapCommandP $ do
    command <- takeWhile1P Nothing (/= '|')

    when (any (`isPrefixOf` command) ["cat ", "echo ", "wc ", "pwd ", "exit "]
         || checkAssignmentViolation command) $
      fail $ "Can't parse " ++ command ++ " command."

    pure $ ExternalCommand command
  where
    checkAssignmentViolation :: String -> Bool
    checkAssignmentViolation command = res
      where
        beforeAssignment = takeWhile (/= '=') command
        evenQuotes       = length (findIndices (== '\"') beforeAssignment) `mod` 2 == 0
        evenBadQuotes    = length (findIndices (== '\'') beforeAssignment) `mod` 2 == 0

        res = beforeAssignment /= command && (evenQuotes || evenBadQuotes)

catP :: Parser Command
catP = wrapCommandP $ do
    _ <- string "cat"

    args <- try (space1 *> argsP) <|> (lookAhead (char delimiter) *> pure [])

    when (length args > 1) $ fail "Too many args in cat command."

    pure $ Cat $ listToMaybe args

wcP :: Parser Command
wcP = wrapCommandP $ do
    _ <- string "wc"

    args <- try (space1 *> argsP) <|> (lookAhead (char delimiter) *> pure [])

    when (length args > 1) $ fail "Too many args in wc command."

    pure $ Wc $ listToMaybe args

pwdP :: Parser Command
pwdP = wrapCommandP $ string "pwd" *> pure Pwd

echoP :: Parser Command
echoP = wrapCommandP $ do
    _ <- string "echo"

    args <- try (space1 *> argsP) <|> (lookAhead (char delimiter) *> pure [])

    pure $ Echo args

exitP :: Parser Command
exitP = wrapCommandP $ string "exit" *> pure Exit

assignmentP :: Parser Command
assignmentP = wrapCommandP $ do
    name <- some alphaNumChar

    when (null name)       $ fail "Can't parse name in assignment operator."

    _ <- char '='

    value <- try (char '\'' *> some (alphaNumChar <|> char ' ') <* char '\'')
             <|> try (char '\"' *> some (alphaNumChar <|> char ' ') <* char '\"')
             <|> some alphaNumChar
    when (null value) $ fail "Can't parse value in assignment operator."

    pure $ Assignment name value

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

wrapCommandP :: Parser Command -> Parser Command
wrapCommandP p = try $ space *> p <* space <* ((() <$ try (char delimiter) <* lookAhead printChar) <|> eof)

argsP :: Parser [String]
argsP = do
  firstArgs <- many (try $ (parseUntilStop False <* space1))
  space
  s <- getInput

  fmap removeQuotes <$> if null s || head s == delimiter then pure firstArgs
                        else (firstArgs ++) <$> try (pure <$> parseUntilStop False)
  where
    removeQuotes :: String -> String
    removeQuotes []  = []
    removeQuotes [x] = [x]
    removeQuotes xs  | head xs == last xs && (head xs == '\"' || head xs == '\'') = init (tail xs)
                     | otherwise                                                  = xs

parseUntilStop :: Bool -> Parser String
parseUntilStop isInQuotes = do
    s <- getInput

    if null s then pure ""
    else do
        let nextChar = head s

        if (nextChar == delimiter || nextChar == ' ') && not isInQuotes
          then
              pure ""
          else do
            nextChar' <- printChar

            let newIsInQuotes = if nextChar' == '\'' || nextChar' == '\"' then not isInQuotes else isInQuotes

            xs <- parseUntilStop newIsInQuotes
            pure $ nextChar' : xs
