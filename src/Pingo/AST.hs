module Pingo.AST where

import Data.List

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

data Example = PosEx String | NegEx String deriving (Eq)
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

instance Show Statement where
  show (StmtConstraint b) = ":- " ++ intercalate ", " (map show b) ++ "."
  show (StmtRule h []) = show h ++ "."
  show (StmtRule h b) = show h ++ " " ++ show (StmtConstraint b)
  show (StmtWeak b t) = ":~ " ++ intercalate ", " (map show b) ++ "." ++ show t
  show (StmtASPFun f b) = f ++ " " ++ show b ++ "."
  show (StmtILASPFun f) = show f

instance Show Head where
  show (Norm l) = show l
  show (Choice a) = show a

instance Show Aggregate where
  show (Count l es u) = bound l ++ " {" ++ intercalate "; " (map show es) ++ "} " ++ bound u
    where bound = maybe "" show

instance Show ChoiceElement where
  show (El l []) = show l
  show (El l nafs) = show l ++ " : " ++ intercalate ", " (map show nafs)

instance Show Literal where
  show (PosLit a) = show a
  show (NegLit a) = "-" ++ show a

instance Show NAFLiteral where
  show (PosNAFLit a) = show a
  show (NegNAFLit a) = "not " ++ show a
  show (BBin t1 op t2) = show t1 ++ show op ++ show t2

instance Show Atom where
  show (Atom name []) = name
  show (Atom name args) = name ++ "(" ++ concatMap show (intersperse (Sep ",") args) ++ ")"

instance Show Term where
  show (Lit atom) = show atom
  show (Num num) = show num
  show (Tuple t) = "(" ++ concatMap show (intersperse (Sep ",") t) ++ ")"
  show (Sep sep) = sep
  show (Str str) = show str
  show (Var v)   = v
  show  AnonVar  = "_"
  show (ABin t1 op t2) = show t1 ++ show op ++ show t2
  show (Assign t1 t2) = show t1 ++ "=" ++ show t2

instance Show BOp where
  show Eq = "=="
  show Neq = "!="
  show Gt = ">"
  show Lt = "<"
  show Gte = ">="
  show Lte = "<="

instance Show AOp where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

instance Show WeakTerms where
  show (Weak w Nothing ts) = "[" ++ show w ++ concatMap ((", " ++) . show) ts ++ "]"
  show (Weak w (Just l) ts) = "[" ++ show w ++ "@" ++ show l ++ concatMap ((", " ++) . show) ts ++ "]"

instance Show OrderingExample where
  show (Brave    ord ex1 ex2) = "#brave_ordering" ++ "(" ++ blankId ord ++ ex1 ++ ", " ++ ex2 ++ ")."
  show (Cautious ord ex1 ex2) = "#cautious_ordering" ++ "(" ++ blankId ord ++ ex1 ++ ", " ++ ex2 ++ ")."
  show (Deep ord ex1 ex2 d) = "#deep_ordering" ++ "(" ++ blankId ord ++ ex1 ++ ", " ++ ex2 ++ ", " ++ show d ++ ")."

instance Show ILASPFun where
  show (Example e) = show e
  show (Ordering o) = show o
  show (Inject m) = "#inject(\"" ++ m ++ "\")."
  show (Child p c) = "#child(" ++ p ++ ", " ++ c ++ ")."
  show (Mode id ts) = id ++ "(" ++ concatMap show (intersperse (Sep ",") ts) ++ ")."

instance Show Example where
  show (PosEx id) = "#pos(" ++ blankId id ++ "{}, {})."
  show (NegEx id) = "#neg(" ++ blankId id ++ "{}, {})."

blankId ""  = ""
blankId id = id ++ ", "

instance Show Program where
  show (Program ss) = intercalate "\n" $ map show ss

