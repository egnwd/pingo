module Pingo.Parser.ASP where

import Pingo.Parser.Utils
import Pingo.AST

import Prelude hiding (head)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Number (sign, decimal)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Data.Either
import Data.Maybe

-- Statements

stmtASP = try (stmtFact <?> "fact")
  <|> try (stmtConstraint <?> "contraint")
  <|> try (stmtRule <?> "rule")
  <|> try (stmtWeak <?> "weak contraint")
  <|> try (stmtASPFun <?> "ASP function")

stmtConstraint :: Parser Statement
stmtConstraint = rulecons *> (StmtConstraint <$> body) <* dot

stmtFact :: Parser Statement
stmtFact = flip StmtRule [] <$> (head <* dot)

stmtRule :: Parser Statement
stmtRule = StmtRule <$> head <* rulecons <*> body <* dot

stmtWeak :: Parser Statement
stmtWeak = wcons *> (StmtWeak <$> body <* dot <*> wts)
  where wts = brackets weightAtLevel <?> "weak constraint terms"

stmtASPFun :: Parser Statement
stmtASPFun = StmtASPFun <$> f <* whiteSpace <*> term <* dot
  where
    f = choice $ map (try . (<* char ' ') . string) aspFuns
    aspFuns = ["#const", "#show"]

-- Rules

head :: Parser Head
head = Norm <$> literal <|> Choice <$> countAgg

body :: Parser [NAFLiteral]
body = whiteSpace *> commaSep nafLiteral

weightAtLevel :: Parser WeakTerms
weightAtLevel = Weak <$> weight <*> level <*> ts
  where
    weight = decimal <?> "weight"
    level = optionMaybe (char '@' *> decimal)
    ts = option [] (comma *> commaSep term)

-- Literals

literal :: Parser Literal
literal =
  ((char '-' *> (NegLit <$> atom))
  <|> (PosLit <$> atom)) <?> "literal"

nafLiteral :: Parser NAFLiteral
nafLiteral =
  try (bExpression <?> "binary expression") <|>
  (reserved "not" *> (NegNAFLit <$> literal))
  <|> (PosNAFLit <$> literal)

-- Terms

term :: Parser Term
term = (tuple <|> str <|> anonvar <|> try aExpression <|> assign <|> subterm) <?> "term"

tuple = Tuple <$> parens (commaSep term)
str = Str <$> parseString
anonvar = char '_' >> return AnonVar
assign = Assign <$> try (subterm <* reservedOp "=") <*> subterm
subterm = (Var <$> variable) <|> (Num <$> (sign <*> decimal)) <|> (Lit <$> atom)
atom = Atom <$> identifier <*> option [] (parens (commaSep term))

-- Binary and Arithmetic Expressions

aExpression :: Parser Term
aExpression = ABin <$> subterm <*> aOperators <*> subterm

bExpression :: Parser NAFLiteral
bExpression = BBin <$> term <*> bOperators <*> term

aOperators :: Parser AOp
aOperators
  =   (reservedOp "*" >> return Multiply)
  <|> (reservedOp "/" >> return Divide  )
  <|> (reservedOp "+" >> return Add     )
  <|> (reservedOp "-" >> return Subtract)

bOperators :: Parser BOp
bOperators
  =   (reservedOp "==" >> return Eq )
  <|> (reservedOp "!=" >> return Neq)
  <|> (reservedOp ">"  >> return Gt )
  <|> (reservedOp "<"  >> return Lt )
  <|> (reservedOp ">=" >> return Gte)
  <|> (reservedOp "<=" >> return Lte)

-- Aggregates

countAgg :: Parser Aggregate
countAgg = Count <$> bound <*> (braces choiceElements <* whiteSpace) <*> bound
  where
    bound = optionMaybe decimal <* whiteSpace

    choiceElements :: Parser [ChoiceElement]
    choiceElements = semiSep1 choiceElement

    choiceElement :: Parser ChoiceElement
    choiceElement = El <$> literal <*> option [] (colon *> many1 nafLiteral)
