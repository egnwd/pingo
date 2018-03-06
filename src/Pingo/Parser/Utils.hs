{-# LANGUAGE FlexibleContexts #-}

module Pingo.Parser.Utils where

import Text.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

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
variable' = (:) <$> upper <*> many (alphaNum <|> char '_' <|> char '\'')
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
