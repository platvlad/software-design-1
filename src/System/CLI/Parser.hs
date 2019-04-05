{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module that is used to parse 'Command's.
--
module System.CLI.Parser
  ( parseCommands
  ) where

import           Control.Applicative             (many, some, (<|>))
import           Control.Monad                   (when)
import           Control.Monad                   (void)
import           Data.Bifunctor                  (first)
import           Data.Functor                    (($>))
import           Data.List                       (findIndices, isPrefixOf)
import           Data.Maybe                      (listToMaybe)
import           System.CLI.Command              (Command (..))
import           System.Console.CmdArgs          (Annotate (..), CmdArgs (..),
                                                  Mode, cmdArgsMode_, record,
                                                  (+=))
import qualified System.Console.CmdArgs          as CA (args, name)
import           System.Console.CmdArgs.Explicit (process)
import           Text.Megaparsec                 (Parsec, customFailure, eof,
                                                  getInput, lookAhead, parse,
                                                  setInput, takeWhile1P, try)
import           Text.Megaparsec.Char            (alphaNumChar, char, printChar,
                                                  space, space1, string)
import           Text.Megaparsec.Error           (ShowErrorComponent (..),
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
      else fmap pure commandP

commandP :: Parser Command
commandP = assignmentP <|> catP <|> wcP <|> pwdP <|> echoP <|> grepP <|> exitP <|> externalP

externalP :: Parser Command
externalP = wrapCommandP $ do
    command <- takeWhile1P Nothing (/= '|')

    when (any (`isPrefixOf` command) ["cat ", "wc ", "pwd ", "echo ", "grep ", "exit "]
          || checkAssignmentViolation command) $ getError command

    pure $ ExternalCommand command
  where
    getError :: String -> Parser ()
    getError command = do
        input <- getInput

        setInput $ command ++ input
        void getError'
      where
        getError' :: Parser Command
        getError' | checkAssignmentViolation command = assignmentP
                  | "cat " `isPrefixOf` command      = catP
                  | "wc " `isPrefixOf` command       = wcP
                  | "pwd " `isPrefixOf` command      = pwdP
                  | "echo " `isPrefixOf` command     = echoP
                  | "grep " `isPrefixOf` command     = grepP
                  | otherwise                        = exitP

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

    args <- try (space1 *> argsP) <|> (aheadStopP *> pure [])

    when (length args > 1) $ fail "Too many args in cat command."

    pure $ Cat $ listToMaybe args

wcP :: Parser Command
wcP = wrapCommandP $ do
    _ <- string "wc"

    args <- try (space1 *> argsP) <|> (aheadStopP *> pure [])

    when (length args > 1) $ fail "Too many args in wc command."

    pure $ Wc $ listToMaybe args

pwdP :: Parser Command
pwdP = wrapCommandP $ string "pwd" *> pure Pwd

echoP :: Parser Command
echoP = wrapCommandP $ do
    _ <- string "echo"

    args <- try (space1 *> argsP) <|> (aheadStopP *> pure [])

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

grepP :: Parser Command
grepP = wrapCommandP $ do
    _ <- string "grep"

    args     <- try (space1 *> argsP) <|> (aheadStopP *> pure [])
    let mode = cmdArgsMode_ grep :: Mode (CmdArgs Command)

    case process mode args of
      Right res -> do
          let command = cmdArgsValue res

          when (length (grArgs command) `notElem` [1, 2]) $ fail "Wrong number of arguments in grep."
          when (grA command < 0)                          $ fail "Negative value of parameter A."

          pure command
      Left e    -> customFailure $ "grep: " ++ e
  where
    grep = record Grep{} [ grI := False += CA.name "i"
                         , grW := False += CA.name "w"
                         , grA := 0 += CA.name "A"
                         , grArgs := [] += CA.args
                         ]

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

aheadStopP :: Parser ()
aheadStopP = (lookAhead (char delimiter) $> ()) <|> (lookAhead eof $> ())

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
