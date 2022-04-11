module Translator where

data ErlangToken
  = ErlAtom String
  | ErlStr String
  | ErlList [ErlangToken]
  | ErlTuple [ErlangToken]
  | ErlInt Integer
  | ErlChar Integer
  | ErlFloat Float
  | ErlBinary String
  | ErlMap [(ErlangToken, ErlangToken)]
  deriving (Show)

class Representation a where
  translate :: a -> ErlangToken

type Abstract = [Form]

data Form
  = FormAttribute Atribute
  | FormFunction Function
  deriving (Show)

instance Representation Form where
  translate (FormAttribute attr) = translate attr
  translate (FormFunction func) = translate func

data Atribute = Atribute
  { attrAnno :: Integer,
    attrName :: String,
    attrValue :: ErlangToken
  }
  deriving (Show)

instance Representation Atribute where
  translate (Atribute anno name value) =
    ErlTuple [ErlAtom "attribute", ErlInt anno, ErlAtom name, value]

data Function = Function
  { funcAnno :: Integer,
    funcName :: String,
    funcArity :: Integer,
    funcClauses :: [FunctionClause]
  }
  deriving (Show)

instance Representation Function where
  translate (Function anno name arity clauses) =
    ErlTuple
      [ ErlAtom "function",
        ErlInt anno,
        ErlAtom name,
        ErlInt arity,
        ErlList (map translate clauses)
      ]

data Literal
  = LiteralAtom Integer String
  | LiteralChar Integer Integer
  | LiteralFloat Integer Float
  | LiteralInteger Integer Integer
  | LiteralString Integer String
  -- TODO binary
  deriving (Show)

instance Representation Literal where
  translate (LiteralAtom anno atom) = ErlTuple [ErlAtom "atom", ErlInt anno, ErlAtom atom]
  translate (LiteralChar anno char) = ErlTuple [ErlAtom "char", ErlInt anno, ErlChar char]
  translate (LiteralFloat anno float) = ErlTuple [ErlAtom "float", ErlInt anno, ErlFloat float]
  translate (LiteralInteger anno i) = ErlTuple [ErlAtom "integer", ErlInt anno, ErlInt i]
  translate (LiteralString anno s) = ErlTuple [ErlAtom "string", ErlInt anno, ErlStr s]

data Pattern
  = PatternAtom Integer String
  | PatternChar Integer Integer
  | PatternFloat Integer Float
  | PatternInteger Integer Integer
  | PatternString Integer String
  | PatternVar Integer String
  | PatternUniversal Integer
  deriving (Show)

instance Representation Pattern where
  translate (PatternAtom anno atom) = ErlTuple [ErlAtom "atom", ErlInt anno, ErlAtom atom]
  translate (PatternChar anno char) = ErlTuple [ErlAtom "char", ErlInt anno, ErlChar char]
  translate (PatternFloat anno float) = ErlTuple [ErlAtom "float", ErlInt anno, ErlFloat float]
  translate (PatternInteger anno i) = ErlTuple [ErlAtom "integer", ErlInt anno, ErlInt i]
  translate (PatternString anno s) = ErlTuple [ErlAtom "string", ErlInt anno, ErlStr s]
  translate (PatternVar anno var) = ErlTuple [ErlAtom "var", ErlInt anno, ErlAtom var]
  translate (PatternUniversal anno) = ErlTuple [ErlAtom "var", ErlInt anno, ErlAtom "_"]

data Guard
  = GuardNil Integer
  | GuardAtom Integer String
  deriving (Show)

-- TODO guards

type GuardSequence = [Guard]

instance Representation Guard where
  translate (GuardNil anno) = ErlTuple [ErlAtom "nil", ErlInt anno]
  translate (GuardAtom anno atom) = ErlTuple [ErlAtom "atom", ErlInt anno, ErlAtom atom]

data FunctionClause = FunctionClause
  { functionClauseAnno :: Integer,
    functionClausePS :: [Pattern],
    functionClauseGS :: GuardSequence,
    functionClauseBody :: Expr
  }
  deriving (Show)

instance Representation FunctionClause where
  translate (FunctionClause anno ps gs body) =
    ErlTuple
      [ ErlAtom "clause",
        ErlInt anno,
        ErlList (map translate ps),
        ErlList (map translate gs),
        ErlList [translate body]
      ]

data CaseClause = CaseClause
  { caseClauseAnno :: Integer,
    caseClausePS :: Pattern,
    caseClauseGS :: GuardSequence,
    caseClauseBody :: Body
  }
  deriving (Show)

instance Representation CaseClause where
  translate (CaseClause anno ps gs body) =
    ErlTuple
      [ ErlAtom "clause",
        ErlInt anno,
        ErlList [translate ps],
        ErlList $ map translate gs,
        ErlList $ map translate body
      ]

type Body = [Expr]

data Expr
  = ExprAtom Integer String
  | ExprFunctionCall
      { exprFunctionCallAnno :: Integer,
        exprFunctionCallFunc :: Expr,
        exprFunctionCallArgs :: [Expr]
      }
  | ExprRemoteCall
      { exprRemoteCallAnno :: Integer,
        exprRemoteCallModule :: Expr,
        exprRemoteCallFunc :: Expr,
        exprRemoteCallArgs :: [Expr]
      }
  | ExprMatch Integer Pattern Expr
  | ExprCase
      { exprCaseAnno :: Integer,
        exprCaseExpr :: Expr,
        exprCaseClauses :: [FunctionClause]
      }
  deriving (Show)

instance Representation Expr where
  translate (ExprAtom anno atom) =
    ErlTuple [ErlAtom "atom", ErlInt anno, ErlAtom atom]
  translate (ExprCase anno expr clauses) =
    ErlTuple [ErlAtom "case", ErlInt anno, translate expr, ErlList $ map translate clauses]
  translate (ExprFunctionCall anno func args) =
    ErlTuple [ErlAtom "call", ErlInt anno, translate func, ErlList $ map translate args]
  translate (ExprRemoteCall anno module' func args) =
    ErlTuple [ErlAtom "remote", ErlInt anno, translate module', translate func, ErlList $ map translate args]
  translate (ExprMatch anno pattern expr) =
    ErlTuple [ErlAtom "match", ErlInt anno, translate pattern, translate expr]

moduleAttr :: Integer -> String -> Atribute
moduleAttr i name = Atribute i "module" (ErlAtom name)

exportAttr :: Integer -> [(String, Integer)] -> Atribute
exportAttr i funcs =
  let pairToErlTuple (s, a) = ErlTuple [ErlAtom s, ErlInt a]
   in Atribute i "export" (ErlList $ map pairToErlTuple funcs)

translateAbstract :: Abstract -> ErlangToken
translateAbstract = ErlList <$> map translate

exampleTmpModule :: Abstract
exampleTmpModule =
  [ FormAttribute $ moduleAttr 1 "sample",
    FormAttribute $ exportAttr 2 [("fun", 0)],
    FormFunction $ Function 3 "fun" 0 [clause]
  ]
  where
    clause = FunctionClause 3 [] [] (ExprAtom 3 "hello")
