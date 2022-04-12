module Lexer where

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Token (makeTokenParser)
import qualified Text.ParserCombinators.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer =
  makeTokenParser
    emptyDef
      { Token.commentLine = "%",
        Token.reservedOpNames = ["+", "-", "/", "*", "++", "--", "->", "::"],
        Token.reservedNames = ["data", "type", "case", "of"]
      }

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
