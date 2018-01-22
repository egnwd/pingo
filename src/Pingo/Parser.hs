{-# LANGUAGE FlexibleContexts #-}

module Pingo.Parser where

import Text.Parsec hiding (uncons,parse)
import qualified Text.Parsec (parse)
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Number

import Options
import Data.Either

import Pingo.AST

-- | The 'commaSep' function is a convenience function
-- for parsing comma separated lists, removing leading and trailing spaces
commaSep = flip sepBy1 (spaces *> char ',' <* spaces)

anyTill p = manyTill anyChar (lookAhead $ try p)

parseString :: Parser String
parseString = do
    char '"'
    strings <- many $ noneOf "\""
    char '"'
    return strings

-- | The 'ident' parser matches text of the form @[a-z\-][a-zA-Z0-9_']*@
ident :: Parser Ident
ident = do
  c <- lower <|> char '-'
  s <- many $ alphaNum <|> char '_' <|> char '\''

  return $ c:s

-- | The 'literal' parser matches either an 'Atom' or a number or a quoted string
literal :: Parser Argument
literal = (Lit <$> atom) <|> tuple <|>
  (Num <$> (sign <*> decimal)) <|> (Str <$> parseString)

-- | The 'tuple' parser matches a tuple of arguments
tuple :: Parser Argument
tuple = do
  t <- between (char '(') (char ')') $ commaSep (literal <?> "a literal")

  return $ Tuple t

-- | The 'atom' parser parses an atom which is either an ident
-- or a function with 'literal' arguments. Some examples are below:
--     * q
--     * p(q, r)
--     * p(s(t), r)
atom :: Parser Atom
atom = do
  name <- ident
  args <- option [] (between (char '(') (char ')') $ commaSep (literal <?> "a literal"))

  return $ Atom name args

-- | The 'answerSet' parser matches a list of 'Atom's
answerSet :: Parser AnswerSet
answerSet = do
  string "Answer: " *> many1 digit <* newline
  (atom <?> "a predicate or atom") `sepEndBy` char ' '

-- | The 'answerSets' parser collects a list of 'AnswerSet's
answerSets :: Parser [AnswerSet]
answerSets = (answerSet <?> "an answer set") `endBy` newline

-- | This 'file' parser removes metadata printed by clingo
-- and returns the inner 'AnswerSet's
file :: Parser [AnswerSet]
file = try (anyTill (string "Answer: ")) *> answerSets
  <|> anyTill (string "UNSATISFIABLE") *> fail "No Answer Sets found"

-- | The 'parse' function takes 'String' input
-- and returns the 'AnswerSet's or a 'ParseError'
parse :: String -> Either ParseError [AnswerSet]
parse = Text.Parsec.parse file "Pingo"
