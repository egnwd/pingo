{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Pingo.Printer (output, Color (..)) where

import System.Console.ANSI hiding (Color)
import Data.List

import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Pingo.AST

data Color = Auto | Always | Never deriving (Bounded, Enum, Eq)

instance Show Color where
  show Auto = "auto"
  show Always = "always"
  show Never = "never"

class Colorable a where
  color :: a -> String

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

printer :: (Colorable a, Show a) => Color -> a -> IO ()
printer c str = do
  istty <- queryTerminal stdOutput
  prettyLn (c == Always || (istty && c /= Never)) str

-- Printers
prettyLn :: (Colorable a, Show a) => Bool -> a -> IO ()
prettyLn True = (>> putStrLn "") . (putStr "\t" >>) . pretty
prettyLn False = (putStr "\t" >>) . print

pretty :: (Colorable a, Show a) => a -> IO ()
pretty = putStr . color

answer n = "Answer " ++ (show n) ++ ":"

output :: Color -> AnswerSet -> IO ()
output c (n, a) = (putStrLn $ answer n) >> mapM_ (printer c) a
