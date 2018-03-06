{-# LANGUAGE RankNTypes #-}

module Pingo.Parser where

import Pingo.AST
import Pingo.Parser.Utils
import Pingo.Parser.ASP
import Pingo.Parser.ILASP

import Text.Parsec hiding (uncons,parse)
import qualified Text.Parsec (parse)
import Text.Parsec.String
import Text.Parsec.Number (sign, decimal)

statements :: Parser [Statement]
statements = many (stmtASP <|> stmtILASP)

-- | This 'file' parser returns the parsed 'Program'
file :: Parser Program
file = whiteSpace *> (Program <$> statements) <* eof

-- | The 'answerSet' parser matches a list of 'Atom's
answerSet :: Parser [Atom]
answerSet = do
  string "Answer: " *> decimal <* newline
  many1 (atom <?> "a predicate or atom")

-- | The 'answerSets' parser collects a list of 'AnswerSet's
answerSets :: Parser [[Atom]]
answerSets = many1 (answerSet <?> "an answer set")

-- | This 'file' parser returns the parsed 'Program'
clingoOut :: Parser [[Atom]]
clingoOut = try (anyTill (string "Answer: ")) *> answerSets
  <|> anyTill (string "UNSATISFIABLE") *> fail "No Answer Sets found"

-- | The 'parse' function takes 'String' input
-- and returns the 'Program' or a 'ParseError'
parse :: forall a . Parser a -> String -> Either ParseError a
parse f = Text.Parsec.parse f "Pingo"

parseFile :: String -> IO (Either ParseError Program)
parseFile = parseFromFile file
