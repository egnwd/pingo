{-# LANGUAGE FlexibleContexts #-}

module Pingo.Parser where

import Text.Parsec hiding (uncons,parse)
import qualified Text.Parsec (parse)
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Number
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
           , Token.identLetter     = alphaNum
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
                                     ,"==" ,"!="
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
rulecons   = symbol ":-"
wcons      = symbol ":~"
commaSep   = Token.commaSep   lexer -- parses comma separated lists
semiSep1   = Token.semiSep1   lexer -- parses semi separated lists

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
  <|> (stmtASPFun <?> "ASP function")

stmtConstraint :: Parser Statement
stmtConstraint = rulecons *> (StmtConstraint <$> body) <* dot

stmtFact :: Parser Statement
stmtFact =
  do h <- headR
     dot
     return $ StmtRule h []

stmtRule :: Parser Statement
stmtRule =
  do h <- headR
     rulecons
     b <- body
     dot
     return $ StmtRule h b

stmtWeak :: Parser Statement
stmtWeak =
  do wcons
     b <- body
     dot
     w <- brackets weightAtLevel <?> "weak constraint terms"
     return $ StmtWeak b w

stmtASPFun :: Parser Statement
stmtASPFun =
  do f <- try (symbol "#const") <|> try (symbol "#show")
     t <- term
     dot
     return $ StmtASPFun f t

weightAtLevel :: Parser WeakTerms
weightAtLevel =
  do w <- decimal <?> "weight"
     l <- optionMaybe (char '@' *> decimal)
     ts <- option [] (comma *> commaSep term)
     return $ Weak w l ts

headR :: Parser Head
headR = Norm <$> literal <|> Choice <$> countAgg

body :: Parser [NAFLiteral]
body = whiteSpace *> commaSep nafLiteral

literal :: Parser Literal
literal =
  (char '-' *> (NegLit <$> atom))
  <|> (PosLit <$> atom)

nafLiteral :: Parser NAFLiteral
nafLiteral =
  try bExpression <|>
  (reserved "not" *> (NegNAFLit <$> literal))
  <|> (PosNAFLit <$> literal)

variable :: Parser Ident
variable = Token.lexeme lexer variable'
  where
    variable' :: Parser Ident
    variable' =
      do c <- upper
         s <- many $ alphaNum <|> char '_' <|> char '\''
         return $ c:s

term :: Parser Term
term =
  Tuple <$> parens (commaSep term) <|> (Str <$> parseString)
  <|> (const AnonVar <$> char '_') <|> try aExpression <|> try assign <|> subterm

assign :: Parser Term
assign =
  do l <- subterm
     whiteSpace *> symbol "=" <* whiteSpace
     r <- subterm
     return $ Assign l r

subterm :: Parser Term
subterm = (Var <$> variable) <|> (Num <$> (sign <*> decimal)) <|> (Lit <$> atom)

atom :: Parser Atom
atom = do
  name <- identifier
  args <- option [] $ parens (commaSep term)

  return $ Atom name args

aExpression :: Parser Term
aExpression =
  do a1 <- subterm
     op <- aOperators
     a2 <- subterm
     return $ ABin op a1 a2

bExpression :: Parser NAFLiteral
bExpression =
  do a1 <- term
     op <- bOperators
     a2 <- term
     return $ BBin op a1 a2

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
countAgg =
  do l <- optionMaybe decimal
     whiteSpace
     els <- braces choiceElements
     whiteSpace
     u <- optionMaybe decimal
     whiteSpace
     return $ Count l u els
  where
    choiceElements :: Parser [ChoiceElement]
    choiceElements = semiSep1 choiceElement

    choiceElement :: Parser ChoiceElement
    choiceElement =
      do l <- literal
         conds <- option [] (colon *> many1 nafLiteral)
         return $ El l conds

-- | This 'file' parser returns the parsed 'Program'
file :: Parser Program
file = whiteSpace *> (Program <$> statements) <* eof

-- | The 'parse' function takes 'String' input
-- and returns the 'Program' or a 'ParseError'
parse :: String -> Either ParseError Program
parse = Text.Parsec.parse file "Pingo"

parseFile :: String -> IO (Either ParseError Program)
parseFile file =
  do contents <- readFile file
     return $ parse contents
