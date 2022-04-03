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

class Erlangable a where
  translate :: a -> ErlangToken

type Abstract = [Form]

data Form
  = FormAttribute Atribute
  | FormFunction Function
  deriving (Show)

instance Erlangable Form where
  translate (FormAttribute attr) = translate attr
  translate (FormFunction func) = translate func

data Atribute = Atribute
  { attrAnno :: Integer,
    attrName :: String,
    attrValue :: ErlangToken
  }
  deriving (Show)

instance Erlangable Atribute where
  translate (Atribute anno name value) =
    ErlTuple [ErlAtom "attribute", ErlInt anno, ErlAtom name, value]

data Function = Function
  { funcAnno :: Integer,
    funcName :: String,
    funcArity :: Integer,
    funcClauses :: [Clause]
  }
  deriving (Show)

instance Erlangable Function where
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

instance Erlangable Literal where
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
  deriving (Show)

instance Erlangable Pattern where
  translate (PatternAtom anno atom) = ErlTuple [ErlAtom "atom", ErlInt anno, ErlAtom atom]
  translate (PatternChar anno char) = ErlTuple [ErlAtom "char", ErlInt anno, ErlChar char]
  translate (PatternFloat anno float) = ErlTuple [ErlAtom "float", ErlInt anno, ErlFloat float]
  translate (PatternInteger anno i) = ErlTuple [ErlAtom "integer", ErlInt anno, ErlInt i]
  translate (PatternString anno s) = ErlTuple [ErlAtom "string", ErlInt anno, ErlStr s]
  translate (PatternVar anno var) = ErlTuple [ErlAtom "var", ErlInt anno, ErlAtom var]

data Guard
  = GuardNil Integer
  | GuardAtom Integer String
  deriving (Show)

-- TODO guards

instance Erlangable Guard where
  translate (GuardNil anno) = ErlTuple [ErlAtom "nil", ErlInt anno]
  translate (GuardAtom anno atom) = ErlTuple [ErlAtom "atom", ErlInt anno, ErlAtom atom]

data Clause = ClauseFunction
  { clauseAnno :: Integer,
    clausePS :: [Pattern],
    clauseGS :: [Guard],
    clauseBody :: Literal -- todo
  }
  deriving (Show)

instance Erlangable Clause where
  translate (ClauseFunction anno ps gs body) =
    ErlTuple
      [ ErlAtom "clause",
        ErlInt anno,
        ErlList (map translate ps),
        ErlList (map translate gs),
        ErlList [translate body]
      ]

moduleAttr :: Integer -> String -> Atribute
moduleAttr i name = Atribute i "module" (ErlAtom name)

exportAttr :: Integer -> [(String, Integer)] -> Atribute
exportAttr i funcs =
  let pairToErlTuple (s, a) = ErlTuple [ErlAtom s, ErlInt a]
   in Atribute i "export" (ErlList $ map pairToErlTuple funcs)

translateAbstract :: Abstract -> [ErlangToken]
translateAbstract = map translate

exampleTmpModule :: Abstract
exampleTmpModule =
  [ FormAttribute $ moduleAttr 1 "sample",
    FormAttribute $ exportAttr 2 [("fun", 0)],
    FormFunction $ Function 3 "fun" 0 [clause]
  ]
  where
    clause = ClauseFunction 3 [] [] (LiteralAtom 3 "hello")
