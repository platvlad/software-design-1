-- | Module that is used to parse 'Command's.
--
module System.CLI.Parser
  ( parseCommands
  ) where

import           Control.Applicative  (many, some, (<|>))
import           Control.Monad        (when)
import           Data.Bifunctor       (first)
import           Data.List            (isPrefixOf)
import           Data.Maybe           (listToMaybe)
import           System.CLI.Command   (Command (..))
import           Text.Megaparsec      (Parsec, customFailure, eof, getInput,
                                       lookAhead, parse, takeWhile1P, try)
import           Text.Megaparsec.Char (alphaNumChar, char, printChar, space,
                                       space1, string)

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
parseCommands = first show . parse commandsP ""

commandsP :: Parser [Command]
commandsP = do
    res <- many commandP

    stream <- getInput

    if null stream
      then pure res
      else customFailure "Can't parse whole command."

commandP :: Parser Command
commandP = assignmentP <|> catP <|> wcP <|> pwdP <|> echoP <|> exitP <|> externalP

externalP :: Parser Command
externalP = wrapCommandP $ do
    command <- takeWhile1P Nothing (/= '|')

    when (any (`isPrefixOf` command) ["cat ", "echo ", "wc ", "pwd ", "exit "]) $
      customFailure $ "Can't parse " ++ command ++ " command."

    pure $ ExternalCommand command

catP :: Parser Command
catP = wrapCommandP $ do
    _ <- string "cat"

    args <- try (space1 *> argsP) <|> (lookAhead (char delimiter) *> pure [])

    when (length args > 1) $ customFailure "Too many args in cat command."

    pure $ Cat $ listToMaybe args

wcP :: Parser Command
wcP = wrapCommandP $ do
    _ <- string "wc"

    args <- try (space1 *> argsP) <|> (lookAhead (char delimiter) *> pure [])

    when (length args > 1) $ customFailure "Too many args in wc command."

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
    when (null name) $ customFailure "Can't parse name in assignment operator."

    _ <- char '='

    value <- try (char '\'' *> some (alphaNumChar <|> char ' ') <* char '\'')
             <|> try (char '\"' *> some (alphaNumChar <|> char ' ') <* char '\"')
             <|> some alphaNumChar
    when (null value) $ customFailure "Can't parse value in assignment operator."

    pure $ Assignment name value

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

wrapCommandP :: Parser Command -> Parser Command
wrapCommandP p = try $ space *> p <* space <* ((const () <$> try (char delimiter)) <|> eof)

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
