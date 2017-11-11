{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Parsec hiding (uncons)
import Text.Parsec.String
import Text.Parsec.Combinator

import Data.List
import System.Console.ANSI

class Colorable a where
  color :: a -> String

type Ident = String
type Number = Int
type AnswerSet = (Int, [Atom])
data Argument = Lit Atom | Num Number | Sep String
data Atom = Atom Ident [Argument]

instance Show Atom where
  show (Atom name []) = name
  show (Atom name args) = name ++ "(" ++ (concatMap show $ intersperse (Sep ", ") args) ++ ")"

instance Show Argument where
  show (Lit atom) = show atom
  show (Num num) = show num
  show (Sep sep) = sep

instance Colorable Ident where
  color i = setSGRCode [SetColor Foreground Vivid Blue] ++ i ++ (setSGRCode [Reset])

instance Colorable Number where
  color n = setSGRCode [SetColor Foreground Dull Yellow] ++ (show n) ++ (setSGRCode [Reset])

instance Colorable Argument where
  color (Lit atom) = color atom
  color (Num n) = color n
  color (Sep s) = s

instance Colorable Atom where
  color (Atom name []) = color name
  color (Atom name args) =
    (setSGRCode [SetColor Foreground Vivid Green]) ++ name ++ (setSGRCode [Reset])
    ++ "(" ++ (concatMap color $ intersperse (Sep ", ") args) ++ ")"

-- Printers
prettyLn :: (Colorable a, Show a) => a -> IO ()
prettyLn = (>> putStrLn "") . (putStr "\t" >>) . pretty

pretty :: (Colorable a, Show a) => a -> IO ()
pretty = putStr . color

output (n, a) = (putStrLn $ "Answer " ++ (show n) ++ ":") >> mapM_ prettyLn a

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

-- Main

main :: IO ()
main = do
  input <- getContents
  case parse file "Clingo Parser" input of
    Left err -> putStrLn "Parse error at:" >> print err
    Right answerSets -> mapM_ output answerSets
