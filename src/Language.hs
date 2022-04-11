module Language where

import Translator (ErlangToken)

data Header = Header
  { moduleName :: String,
    attributes :: [(String, String)]
    -- attributes :: [ErlangToken]
  }
  deriving (Show)

-- data HFunction = HFunction
--   { hFuncLine :: Int,
--     hFuncName :: String,
--     hFuncArgs :: [String]
--   }
--   deriving (Show)

-- data Expression

