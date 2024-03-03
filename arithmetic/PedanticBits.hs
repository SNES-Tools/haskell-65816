{-
  This is a language of bits, using a more sophisticated type system.
-}
{-
  Syntax:
  e ::= n
      | ~e
      | extend e
      | sign-extend e
      | shrink e
      | e & e
      | e | e
      | e ^ e
      | e + e
-}
data Expr
  = Lit Int
  | BinOp BinOp Expr Expr
  | UnaryOp UnaryOp Expr
  | Let Id Type Expr Expr
  | Var Id
  deriving (Show)

data UnaryOp
  = BitNot
  | Extend
  | SignExtend
  | Shrink
  deriving (Show)

data BinOp
  = BitOp
  | ArithOp
  deriving (Show)

data BitOp
  = BitAnd
  | BitOr
  | BitEor
  deriving (Show)

data ArithOp =
  ArithPlus
  deriving (Show)

type Id = String

{-
  Typing rules:

  on a sheet of paper
-}
type Context = [(Id, Type)]

data Type =
  BitType BitParam
  deriving (Show)

data BitParam
  = Exactly Word -- = n
  | AtLeast Word -- >= n
  | AtMost Word -- <= n
  deriving (Show)

-- relation from TAPL
-- T1 <: T2 if and only if T1 is compatible with T2
(<:) :: Type -> Type -> Bool
(<:) (BitType (Exactly n)) (BitType (Exactly m)) = n == m
(<:) _ _ = undefined

extend :: Context -> Id -> Type -> Context
extend c id t = (id, t) : c

typeof :: Expr -> Type
typeof = typeof' []

typeof' :: Context -> Expr -> Type
typeof' _ (Lit i)
  | i > 0 = BitType (Exactly (ceiling $ logBase 2 $ (fromIntegral i) + 1))
  | i == 0 = BitType (Exactly 0)
  | i < 0 = BitType (Exactly (ceiling $ logBase 2 $ (abs $ fromIntegral i)))
typeof' c (UnaryOp BitNot e) = typeof' c e
typeof' c (UnaryOp Extend e) =
  case typeof' c e of
    BitType (Exactly n) -> BitType (AtLeast n)
    BitType (AtLeast n) -> BitType (AtMost n)
typeof' c (UnaryOp SignExtend e) = typeof' c e
typeof' c (UnaryOp BitNot e) = typeof' c e
typeof' _ _ = undefined
-- stuck, giving up for now
