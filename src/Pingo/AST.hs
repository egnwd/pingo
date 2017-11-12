module Pingo.AST where

import Data.List

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
