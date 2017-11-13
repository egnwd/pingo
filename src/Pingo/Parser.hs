{-# LANGUAGE FlexibleContexts #-}

module Pingo.Parser where

import Text.Parsec hiding (uncons,parse)
import qualified Text.Parsec (parse)
import Text.Parsec.String
import Text.Parsec.Combinator

import Options

import Pingo.AST

-- | The 'commaSep' function is a convenience function
-- for parsing comma separated lists, removing leading and trailing spaces
commaSep = flip sepBy1 (spaces *> char ',' <* spaces)

-- | The 'skipLines' function ignores 'n' arbitrary lines of text
-- It takes 1 input 'n'
skipLines :: Stream s m Char => Int -> ParsecT s u m [Char]
skipLines n = count n $ (many $ noneOf "\n") *> newline

-- | The 'ident' parser matches text of the form @[a-z][a-zA-Z0-9_']*@
ident :: Parser Ident
ident = do
  c <- lower
  s <- many $ alphaNum <|> (char '_') <|> (char '\'')

  return $ c:s

-- | The 'literal' parser matches either an 'Atom' or a number
literal :: Parser Argument
literal = (Lit <$> atom) <|> (tuple) <|> (Num <$> read <$> many1 digit)

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
  string "Answer: " *> (many1 digit) <* newline
  return =<< (atom <?> "a predicate or atom") `sepBy` (char ' ')

-- | The 'answerSets' parser collects a list of 'AnswerSet's
answerSets :: Parser [AnswerSet]
answerSets = (answerSet <?> "an answer set") `endBy` newline

-- | This 'file' parser removes metadata printed by clingo
-- and returns the inner 'AnswerSet's
file :: Parser [AnswerSet]
file = skipLines 3 *> answerSets <* skipLines 6 <* eof

-- | The 'parse' function takes 'String' input
-- and returns the 'AnswerSet's or a 'ParseError'
parse :: String -> Either ParseError [AnswerSet]
parse input = Text.Parsec.parse file "Pingo" input
