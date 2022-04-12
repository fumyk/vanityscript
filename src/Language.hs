module Language where

import Translator (ErlangToken)

data VModule = VModule
  { moduleName2 :: String,
    attributes2 :: [ErlangToken],
    functions :: [VFunction]
  }
  deriving (Show)

data VFunction = VFunction
  { vfname :: String,
    vfargs :: [ErlangToken]
  }
  deriving (Show)

data Keyword
  = KeywordType
  | KeywordData
  | KeywordPort
  deriving (Show)

data Ident
  = IdentKeyword Keyword
  | IdentAtom String
  | IdentType String
  | IdentVariable String
  deriving (Show)
