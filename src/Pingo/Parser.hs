{-# LANGUAGE FlexibleContexts #-}

module Pingo.Parser (parse) where

import Text.Parsec hiding (uncons,parse)
import qualified Text.Parsec (parse)
import Text.Parsec.String
import Text.Parsec.Combinator

import Pingo.AST

-- Parser Helpers
commaSep = flip sepBy1 (spaces *> char ',' <* spaces)

skipLines :: Stream s m Char => Int -> ParsecT s u m [Char]
skipLines n = count n $ (many $ noneOf "\n") *> newline

-- Main Parsers
ident :: Parser Ident
ident = do
  c <- lower
  s <- many $ alphaNum <|> (char '_') <|> (char '\'')

  return $ c:s

literal :: Parser Argument
literal = (Lit <$> atom) <|> (Num <$> read <$> many1 digit)

atom :: Parser Atom
atom = do
  name <- ident
  args <- option [] (between (char '(') (char ')') $ commaSep (literal <?> "a literal"))

  return $ Atom name args

answerSet :: Parser AnswerSet
answerSet = do
  n <- string "Answer: " *> (read <$> many1 digit) <* newline
  atoms <- (atom <?> "a predicate or atom") `sepBy` (char ' ')

  return (n, atoms)

answerSets :: Parser [AnswerSet]
answerSets = (answerSet <?> "an answer set") `endBy` newline

file :: Parser [AnswerSet]
file = skipLines 3 *> answerSets <* skipLines 6 <* eof

parse :: String -> Either ParseError [AnswerSet]
parse input = Text.Parsec.parse file "Pingo" input
