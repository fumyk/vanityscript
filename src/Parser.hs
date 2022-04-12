-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Parser where

import qualified Data.ByteString.Char8 as C
import Data.Functor.Identity (Identity)
import Language
import Lexer
import Text.Parsec (ParsecT, parseTest, sepBy)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    digit,
    letter,
    many,
    many1,
    newline,
    space,
    spaces,
    string,
    (<|>),
  )

-- va :: ParsecT String u Identity String
-- va = many1 (letter <|> digit)

-- pAttr :: ParsecT String u Identity (String, String)
-- pAttr = (,) <$> (va <* space <* spaces) <*> va

-- pModule :: ParsecT String u Identity Header
-- pModule = Header <$> (string "module" *> space *> spaces *> va <* newline) <*> sepBy pAttr newline

parseModule :: Parser String
parseModule = string "module" *> space *> spaces *> identifier
