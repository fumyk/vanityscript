-- {-# LANGUAGE OverloadedStrings #-}

module VanityParser where

import qualified Data.ByteString.Char8 as C
import Data.Functor.Identity (Identity)
import Language (Header (Header))
import Text.Parsec (ParsecT, sepBy)
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
import Text.ParserCombinators.Parsec.Combinator (chainr)

va :: ParsecT String u Identity String
va = many1 (letter <|> digit)

pAttr :: ParsecT String u Identity (String, String)
pAttr = do
  n <- va
  space *> spaces
  v <- va
  return (n, v)

-- pModule :: Parser Header
-- pModule = do
--   string "module" *> space *> spaces
--   name <- va
--   newline
--   attr <- many pAttr
--   return $ Header name attr

pModule' :: ParsecT String u Identity Header
pModule' = Header <$> (string "module" *> space *> spaces *> va <* newline) <*> sepBy pAttr newline
