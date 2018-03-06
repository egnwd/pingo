module Pingo.Parser.ILASP where

import Pingo.Parser.ASP
import Pingo.Parser.Utils
import Pingo.AST

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Number (decimal)
import Text.ParserCombinators.Parsec.Expr

import Data.Either
import Data.Maybe

stmtILASP = StmtILASPFun <$> stmtILASPFun <?> "ILASP function"

stmtILASPFun :: Parser ILASPFun
stmtILASPFun = example <|> ordering <|> child <|> modeDeclaration

example :: Parser ILASPFun
example = Example <$> (negex <|> posex)
  where
    negex =
      do try (reserved "#neg")
         (id, inc, exc, ctx) <- parens innerex
         dot
         return $ NegEx id inc exc ctx
    posex =
      do try (reserved "#pos")
         (id, inc, exc, ctx) <- parens innerex
         dot
         return $ PosEx id inc exc ctx
    innerex =
      do id <- option "" (identifier <* comma)
         optional newline <* whiteSpace
         inc <- setOf atom
         whiteSpace *> comma <* whiteSpace
         exc <- setOf atom
         ctx <- option [] (comma *> braces (many (stmtASP <|> stmtILASP)))
         return (id, inc, exc, ctx)

ordering :: Parser ILASPFun
ordering = Ordering <$> (brave <|> cautious <|> deep)
  where
    brave =
      do try (reserved "#brave_ordering")
         (id, ex1, ex2) <- parens innerord
         dot
         return $ Brave id ex1 ex2
    cautious =
      do try (reserved "#cautious_ordering")
         (id, ex1, ex2) <- parens innerord
         dot
         return $ Cautious id ex1 ex2
    deep =
      do try (reserved "#deep_ordering")
         (id, ex1, ex2, d) <- parens innerordDeep
         dot
         return $ Deep id ex1 ex2 d
    innerord =
      do id <- identifier <* comma
         ex1 <- identifier
         ex2 <- option "" (comma *> identifier)
         return $ if ex2 == "" then (ex2, id, ex1) else (id, ex1, ex2)
    innerordDeep =
      do (id, ex1, ex2) <- innerord
         d <- comma *> decimal
         return (id, ex1, ex2, d)

child :: Parser ILASPFun
child =
  do try (reserved "#child")
     (p, c) <- parens ((,) <$> (identifier <* comma) <*> identifier)
     dot
     return $ Child p c

modeDeclaration :: Parser ILASPFun
modeDeclaration = Mode <$> choice (map (try . string) modeDecFuns) <*> parens (commaSep term) <* dot
  where
    modeDecFuns = ["#modeh", "#modeha", "#modeb", "#modeo", "#constant", "#maxp", "#maxv", "#weight"]
