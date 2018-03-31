{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Pingo.Printer.Color (
  Color (..),
  Colorable (..)
) where

import System.Console.ANSI hiding (Color)
import Data.List

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

instance Colorable Term where
  color (Lit atom) = color atom
  color (Num n) = color n
  color (Str s) = setSGRCode [SetColor Foreground Vivid Yellow] ++ show s ++ setSGRCode [Reset]
  color (Tuple t) = "(" ++ concatMap color (intersperse (Sep ", ") t) ++ ")"
  color (Sep s) = s
  color x = show x

instance Colorable Atom where
  color (Atom name []) = color name
  color (Atom name args) =
    setSGRCode [SetColor Foreground Vivid Green] ++ name ++ setSGRCode [Reset]
    ++ "(" ++ concatMap color (intersperse (Sep ", ") args) ++ ")"

instance Colorable AnswerSet where
  color (AnswerSet as n o) = "Answer " ++ show n ++ ":" ++ maybe "" color o ++ "\n"
    ++ intercalate "\n" (map (("\t" ++) . color) as)

instance Colorable Optimisation where
  color = show
