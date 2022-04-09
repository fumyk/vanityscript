module Main where

import Translator
import Text.Parsec.ByteString (Parser)
import Text.Parsec (string, parseTest)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = print $ translateAbstract exampleTmpModule

type Kw = String 

tmpTestParser :: Parser Kw
tmpTestParser = string "yo"

main2 :: IO ()
main2 =
    parseTest tmpTestParser (C.pack "yo qwe")
