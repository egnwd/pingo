module Pingo.AST where

import Data.List

type Ident = String
type Number = Int

data Program = Program [Statement] deriving (Show)

data Statement
  = StmtConstraint [NAFLiteral]
  | StmtRule Literal [NAFLiteral]
  | StmtWeak [NAFLiteral] WeakTerms
  deriving (Show)

type Weight = Int
type Level = Int
data WeakTerms = Weak Weight (Maybe Level) [Term] deriving (Show)

data NAFLiteral
  = PosNAFLit Literal
  | NegNAFLit Literal
  | BBin BOp Term Term
  deriving (Show)

data Literal = PosLit Atom | NegLit Atom deriving (Show)
data Atom = Atom Ident [Term] deriving (Show)
data Term
  = Lit Atom
  | Num Number
  | Str String
  | Var String
  | AnonVar
  | Tuple [Term]
  | Sep String
  | ABin AOp Term Term
  deriving (Show)

data BOp = Eq
           | Neq
           | Gt
           | Lt
           | Gte
           | Lte
           deriving (Show)

data AOp = Add
           | Subtract
           | Multiply
           | Divide
           deriving (Show)

{- instance Show Atom where -}
  {- show (Atom name []) = name -}
  {- show (Atom name args) = name ++ "(" ++ concatMap show (intersperse (Sep ",") args) ++ ")" -}

{- instance Show Term where -}
  {- show (Lit atom) = show atom -}
  {- show (Num num) = show num -}
  {- show (Tuple t) = "(" ++ concatMap show (intersperse (Sep ",") t) ++ ")" -}
  {- show (Sep sep) = sep -}
