module Pingo.AST.Types where

type Ident = String
type Number = Int

data Program = Program [Statement]

data Statement
  = StmtConstraint [NAFLiteral]
  | StmtRule Head [NAFLiteral]
  | StmtWeak [NAFLiteral] WeakTerms
  | StmtASPFun Ident Term
  | StmtILASPFun ILASPFun
  deriving (Eq)

data ILASPFun
  = Example Example
  | Ordering OrderingExample
  | Child String String
  | Inject String
  | Mode Ident [Term]
  deriving (Eq)

data Example
  = PosEx String [Atom] [Atom] [Statement]
  | NegEx String [Atom] [Atom] [Statement]
  deriving (Eq)
data OrderingExample
  = Brave String String String
  | Cautious String String String
  | Deep String String String Int
  deriving (Eq)

type Weight = Int
type Level = Int
data WeakTerms = Weak Weight (Maybe Level) [Term] deriving (Eq)

data NAFLiteral
  = PosNAFLit Literal
  | NegNAFLit Literal
  | BBin Term BOp Term
  deriving (Eq)

data Literal = PosLit Atom | NegLit Atom deriving (Eq)
data Head = Norm Literal | Choice Aggregate deriving (Eq)
data Aggregate = Count (Maybe Int) [ChoiceElement] (Maybe Int) deriving (Eq)
data ChoiceElement = El Literal [NAFLiteral] deriving (Eq)
data Atom = Atom Ident [Term] deriving (Eq)
data Term
  = Lit Atom
  | Num Number
  | Str String
  | Var String
  | AnonVar
  | Tuple [Term]
  | Sep String
  | ABin Term AOp Term
  | Assign Term Term
  deriving (Eq)

data BOp = Eq
         | Neq
         | Gt
         | Lt
         | Gte
         | Lte
         deriving (Eq)

data AOp = Add
         | Subtract
         | Multiply
         | Divide
         deriving (Eq)

