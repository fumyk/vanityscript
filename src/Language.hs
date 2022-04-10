module Language where

data Header
  = Header {
      moduleName :: String,
      attributes :: [(String, String)]
  }
  deriving Show

-- data Expression 