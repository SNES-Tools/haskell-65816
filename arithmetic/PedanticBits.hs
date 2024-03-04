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
      | let x = e in e
      | x
-}
data Expr
  = Lit Int
  | BinaryOp BinaryOp Expr Expr
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

data BinaryOp
  = BitOp BitOp
  | ArithOp ArithOp
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
  BitType Word (Bound Word) -- lower bound, upper bound

instance Show Type where
  show (BitType m (Finite n))
    | m == n = "bit[" ++ show n ++ "]"
    | otherwise = "bit[n] where " ++ show m ++ "<=n<=" ++ show n
  show (BitType m Infinity) = "bit[n] where n>=" ++ show m

data Bound a -- this is just the Maybe type, but with cool names
  = Finite a
  | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Bound a) where
  (<=) (Finite _) Infinity = True
  (<=) (Finite a) (Finite b) = a <= b
  (<=) Infinity _ = False
  (<=) Infinity Infinity = True

-- relation from TAPL
-- T1 <: T2 if and only if T1 is compatible with T2
(<:) :: Type -> Type -> Bool
--(<:) (BitType (Exactly n)) (BitType (Exactly m)) = n == m
(<:) _ _ = undefined

extend :: Context -> Id -> Type -> Context
extend c id t = (id, t) : c

typeof :: Expr -> Type
typeof = typeof' []

typeof' :: Context -> Expr -> Type
typeof' _ (Lit i)
  | i > 0 =
    let bits = ceiling $ logBase 2 $ (fromIntegral i) + 1
     in constant bits
  | i == 0 = constant 0
  | i < 0 =
    let bits = ceiling $ logBase 2 $ (abs $ fromIntegral i)
     in constant bits
  where
    constant x = BitType x (Finite x)
typeof' c (UnaryOp BitNot e) = typeof' c e
{-
  extend constant   [n,n]   => [n,inf)
  shrink bounded    [m,n]   => [m,inf)
  extend lower bnd  [n,inf) => [n,inf)
  extend upper bnd  [0,n]   => [0,inf)
  extend unbounded  [0,inf) => [0,inf)
-}
typeof' c (UnaryOp Extend e) =
  case typeof' c e of
    BitType lb _ -> BitType lb Infinity
typeof' c (UnaryOp SignExtend e) =
  case typeof' c e of
    BitType lb _ -> BitType lb Infinity
{-
  shrink constant   [n,n]   => [0,n]
  shrink bounded    [m,n]   => [0,n]
  shrink lower bnd  [n,inf) => [0,inf)
  shrink upper bnd  [0,n]   => [0,n]
  shrink unbounded  [0,inf) => [0,inf)
-}
typeof' c (UnaryOp Shrink e) =
  case typeof' c e of
    BitType _ ub -> BitType 0 ub
{-
  binary operator typing rules
  * the general idea: the sizes should match!
  * compute the intersection of the possible ranges of the types
  * the size of the type is the intersection of the ranges
  * it will fail to typecheck if the sizes are disjoint!
-}
typeof' c (BinaryOp _ e1 e2) =
  let (t1, t2) = (typeof' c e1, typeof' c e2)
   in intersect t1 t2
  where
    intersect (BitType lb1 ub1) (BitType lb2 ub2) =
      if (Finite lb2) > ub1 || (Finite lb1) > ub2
        then error "Incompatible types in binary operation"
        else BitType (max lb1 lb2) (min ub1 ub2)

example :: Expr
example =
  BinaryOp (BitOp BitAnd) (UnaryOp Extend (Lit 5)) (UnaryOp Shrink (Lit 50))
