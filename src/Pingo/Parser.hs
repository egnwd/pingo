{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Pingo.Parser where

import Text.Parsec hiding (uncons,parse)
import qualified Text.Parsec (parse)
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Number (sign, decimal)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Data.Either
import Data.Maybe

import Pingo.AST

languageDef =
  emptyDef { Token.commentStart    = "%*"
           , Token.commentEnd      = "*%"
           , Token.commentLine     = "%"
           , Token.identStart      = lower
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = [ "#child"
                                     , "#pos"
                                     , "#neg"
                                     , "#brave_ordering"
                                     , "#cautious_ordering"
                                     , "#deep_ordering"
                                     , "#show"
                                     , "#const"
                                     , "not"
                                     ]
           , Token.reservedOpNames = ["+" ,"-" ,"*" ,"/"
                                     ,"==" ,"!=", "="
                                     ,"<" ,">"
                                     ,">=" ,"<="
                                     ]
           }

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding ( )
brackets   = Token.brackets   lexer -- parses surrounding [ ]
braces     = Token.braces     lexer -- parses surrounding { }
integer    = Token.integer    lexer -- parses an integer
symbol     = Token.symbol     lexer
comma      = Token.comma      lexer -- parses a comma
colon      = Token.colon      lexer -- parses a colon
semi       = Token.semi       lexer -- parses a semicolon
dot        = Token.dot        lexer -- parses a dot
whiteSpace = Token.whiteSpace lexer -- parses whitespace
variable   = Token.lexeme     lexer variable' -- parses variable
rulecons   = symbol ":-"
wcons      = symbol ":~"
commaSep   = Token.commaSep   lexer -- parses comma separated lists
semiSep1   = Token.semiSep1   lexer -- parses semi separated lists

anyTill p = manyTill anyChar (lookAhead $ try p)

setOf p = braces (commaSep p)

parseString :: Parser String
parseString = do
    char '"'
    strings <- many $ noneOf "\""
    char '"'
    return strings

statements :: Parser [Statement]
statements = many statement

statement :: Parser Statement
statement = try (stmtFact <?> "fact")
  <|> try (stmtConstraint <?> "contraint")
  <|> try (stmtRule <?> "rule")
  <|> (stmtWeak <?> "weak contraint")
  <|> try (stmtASPFun <?> "ASP function")
  <|> (StmtILASPFun <$> stmtILASPFun <?> "ILASP function")

stmtConstraint :: Parser Statement
stmtConstraint = rulecons *> (StmtConstraint <$> body) <* dot

stmtFact :: Parser Statement
stmtFact = flip StmtRule [] <$> (headR <* dot)

stmtRule :: Parser Statement
stmtRule = StmtRule <$> headR <* rulecons <*> body <* dot

stmtWeak :: Parser Statement
stmtWeak = wcons *> (StmtWeak <$> body <* dot <*> wts)
  where wts = brackets weightAtLevel <?> "weak constraint terms"

stmtASPFun :: Parser Statement
stmtASPFun = StmtASPFun <$> f <* whiteSpace <*> term <* dot
  where
    f = choice $ map (try . (<* char ' ') . string) aspFuns
    aspFuns = ["#const", "#show"]

weightAtLevel :: Parser WeakTerms
weightAtLevel = Weak <$> weight <*> level <*> ts
  where
    weight = decimal <?> "weight"
    level = optionMaybe (char '@' *> decimal)
    ts = option [] (comma *> commaSep term)

headR :: Parser Head
headR = Norm <$> literal <|> Choice <$> countAgg

body :: Parser [NAFLiteral]
body = whiteSpace *> commaSep nafLiteral

literal :: Parser Literal
literal =
  ((char '-' *> (NegLit <$> atom))
  <|> (PosLit <$> atom)) <?> "literal"

nafLiteral :: Parser NAFLiteral
nafLiteral =
  try (bExpression <?> "binary expression") <|>
  (reserved "not" *> (NegNAFLit <$> literal))
  <|> (PosNAFLit <$> literal)

variable' :: Parser Ident
variable' = (:) <$> upper <*> many (alphaNum <|> char '_' <|> char '\'')

term :: Parser Term
term = (tuple <|> str <|> anonvar <|> try aExpression <|> assign <|> subterm) <?> "term"

tuple = Tuple <$> parens (commaSep term)
str = Str <$> parseString
anonvar = char '_' >> return AnonVar
assign = Assign <$> try (subterm <* reservedOp "=") <*> subterm
subterm = (Var <$> variable) <|> (Num <$> (sign <*> decimal)) <|> (Lit <$> atom)
atom = Atom <$> identifier <*> option [] (parens (commaSep term))

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

countAgg :: Parser Aggregate
countAgg = Count <$> bound <*> (braces choiceElements <* whiteSpace) <*> bound
  where
    bound = optionMaybe decimal <* whiteSpace

    choiceElements :: Parser [ChoiceElement]
    choiceElements = semiSep1 choiceElement

    choiceElement :: Parser ChoiceElement
    choiceElement = El <$> literal <*> option [] (colon *> many1 nafLiteral)


-- ILASP

stmtILASPFun :: Parser ILASPFun
stmtILASPFun = example <|> ordering <|> child <|> modeDeclaration

example :: Parser ILASPFun
example = Example <$> (negex <|> posex)
  where
    negex =
      do try (reserved "#neg")
         (id, _, _, _) <- parens innerex
         dot
         return $ NegEx id
    posex =
      do try (reserved "#pos")
         (id, _, _, _) <- parens innerex
         dot
         return $ PosEx id
    innerex =
      do id <- option "" (identifier <* comma)
         optional newline
         whiteSpace *> braces (many $ noneOf "}")
         whiteSpace *> comma <* whiteSpace
         braces (many $ noneOf "}")
         optional (comma *> braces (many $ noneOf "}"))
         return (id, [], [], [])

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

-- | This 'file' parser returns the parsed 'Program'
file :: Parser Program
file = whiteSpace *> (Program <$> statements) <* eof

-- | The 'answerSet' parser matches a list of 'Atom's
answerSet :: Parser [Atom]
answerSet = do
  string "Answer: " *> decimal <* newline
  many1 (atom <?> "a predicate or atom")

-- | The 'answerSets' parser collects a list of 'AnswerSet's
answerSets :: Parser [[Atom]]
answerSets = many1 (answerSet <?> "an answer set")

-- | This 'file' parser returns the parsed 'Program'
clingoOut :: Parser [[Atom]]
clingoOut = try (anyTill (string "Answer: ")) *> answerSets
  <|> anyTill (string "UNSATISFIABLE") *> fail "No Answer Sets found"

-- | The 'parse' function takes 'String' input
-- and returns the 'Program' or a 'ParseError'
parse :: forall a . Parser a -> String -> Either ParseError a
parse f = Text.Parsec.parse f "Pingo"

parseFile :: String -> IO (Either ParseError Program)
parseFile f =
  do contents <- readFile f
     return $ parse file contents
