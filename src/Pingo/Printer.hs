{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Pingo.Printer (output, printer, Color (..), Colorable (..)) where

import System.Console.ANSI hiding (Color)
import Data.List

import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Pingo.AST

data Color
  = Auto -- ^ 'Auto' colors the output depending on if the output is to a terminal or not
  | Always -- ^ 'Always' will always color the output, even if piped or redirected
  | Never -- ^ 'Never' will not color the output, even if it's to a terminal
  deriving (Bounded, Enum, Eq)

instance Show Color where
  show Auto = "auto"
  show Always = "always"
  show Never = "never"

class Colorable a where
  -- | 'color' is synonymous with 'show'
  -- but the string may contain ANSI colors from "System.Console.ANSI"
  color :: a -> String

instance Colorable Ident where
  color i = setSGRCode [SetColor Foreground Vivid Blue] ++ i ++ setSGRCode [Reset]

instance Colorable Number where
  color n = setSGRCode [SetColor Foreground Dull Yellow] ++ show n ++ setSGRCode [Reset]

instance Colorable Argument where
  color (Lit atom) = color atom
  color (Num n) = color n
  color (Tuple t) = "(" ++ concatMap color (intersperse (Sep ", ") t) ++ ")"
  color (Sep s) = s

instance Colorable Atom where
  color (Atom name []) = color name
  color (Atom name args) =
    setSGRCode [SetColor Foreground Vivid Green] ++ name ++ setSGRCode [Reset]
    ++ "(" ++ concatMap color (intersperse (Sep ", ") args) ++ ")"

printer :: (Colorable a, Show a) => Color -> a -> IO ()
printer c str = do
  istty <- queryTerminal stdOutput
  pretty (c == Always || (istty && c /= Never)) str

-- | The 'pretty' printer prints an element prefixing it with a tab,
-- and either with or without color
pretty :: (Colorable a, Show a) => Bool -> a -> IO ()
pretty colored
  | colored = (>> putStrLn "") . (putStr "\t" >>) . putStr . color
  | otherwise = (putStr "\t" >>) . print

answer n = "Answer " ++ show n ++ ":"


-- | The 'output' function takes an ID-Answer Set tuple and prints it according to the 'Color' option
output :: Color -> (Int, AnswerSet) -> IO ()
output c (n, a) = putStrLn (answer n) >> mapM_ (printer c) a
