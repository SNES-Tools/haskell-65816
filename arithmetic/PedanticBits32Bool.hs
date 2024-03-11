import Data.Bits
import Data.Word (Word32)

{-
  This is a language of bits, using a more sophisticated type system.

  In this version, the maximum width of the bits type is 32.
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
      | if e then e else e
      | e and e
      | e or e
      | not e
      | e = e     -- integer comparison only
-}
data Expr
  = LitInt Int
  | LitBool Bool
  | BinaryOp BinaryOp Expr Expr
  | UnaryOp UnaryOp Expr
  | Let Id TypeSyntax Expr Expr
  | If Expr Expr Expr
  | Var Id
  deriving (Show)

data UnaryOp
  = BitNot
  | Extend
  | SignExtend
  | Shrink
  | BoolNot
  deriving (Show)

data BinaryOp
  = BitOp BitOp
  | ArithOp ArithOp
  | BoolOp BoolOp
  | CompOp CompOp
  deriving (Show)

data BitOp
  = BitAnd
  | BitOr
  | BitEor
  deriving (Show)

data ArithOp =
  ArithPlus
  deriving (Show)

data CompOp =
  CompEqual
  deriving (Show)

data BoolOp
  = BoolAnd
  | BoolOr
  deriving (Show)

type Id = String

data TypeSyntax
  = BitTypeSyntax Word -- should be <= 32, not checked
  -- written type should be exact, range only applies internally
  | BoolTypeSyntax
  deriving (Show)

{-
  Typing rules:

  now in my mind...
-}
type Context = [(Id, Type)]

data Type
  = BitType Word Word -- lower bound, upper bound
  | BoolType

maxBits :: Word
maxBits = 32

instance Show Type where
  show (BitType m n)
    | m == n = "bit[" ++ show n ++ "]"
    | otherwise = "bit[n] where " ++ show m ++ "<=n<=" ++ show n
  show BoolType = "bool"

extend :: Context -> Id -> Type -> Context
extend c id t = (id, t) : c

typeof :: Expr -> Type
typeof = typeof' []

typeof' :: Context -> Expr -> Type
typeof' _ (LitInt i)
  | i > 0 =
    let bits = ceiling $ logBase 2 $ (fromIntegral i) + 1
     in constant (max bits 1) -- don't allow 0 wide bit types anymore
  | i == 0 = constant 1
  | i < 0 =
    let bits = ceiling $ logBase 2 $ (abs $ fromIntegral i)
     in constant (max bits 1)
  where
    constant x = BitType x x
typeof' _ (LitBool _) = BoolType
typeof' c (UnaryOp BitNot e) =
  case typeof' c e of
    t@(BitType _ _) -> t
    _ -> error "Type error: bitwise not"
typeof' c (UnaryOp BoolNot e) =
  case typeof' c e of
    t@BoolType -> t
    _ -> error "Type error: boolean not"
{-
  extend constant   [n,n]   => [n,32]
  shrink bounded    [m,n]   => [m,32]
  extend lower bnd  [n,32]  => [n,32]
  extend upper bnd  [0,n]   => [0,32]
  extend unbounded  [0,32]  => [0,32]
-}
typeof' c (UnaryOp Extend e) =
  case typeof' c e of
    BitType lb _ -> BitType lb maxBits
    _ -> error "Type error: extend"
typeof' c (UnaryOp SignExtend e) =
  case typeof' c e of
    BitType lb _ -> BitType lb maxBits
    _ -> error "Type error: sign-extend"
{-
  shrink constant   [n,n]   => [0,n]
  shrink bounded    [m,n]   => [0,n]
  shrink lower bnd  [n,32]  => [0,32]
  shrink upper bnd  [0,n]   => [0,n]
  shrink unbounded  [0,32]  => [0,32]
-}
typeof' c (UnaryOp Shrink e) =
  case typeof' c e of
    BitType _ ub -> BitType 1 ub
    _ -> error "Type error: shrink"
{-
  binary operator typing rules
  * the general idea: the sizes should match!
  * compute the intersection of the possible ranges of the types
  * the size of the type is the intersection of the ranges
  * it will fail to typecheck if the sizes are disjoint!
-}
typeof' c (BinaryOp (BoolOp _) e1 e2) =
  case (typeof' c e1, typeof' c e2) of
    (BoolType, BoolType) -> BoolType
    (_, _) -> error "Type error: binary operation with booleans"
typeof' c (BinaryOp (CompOp _) e1 e2) =
  case (typeof' c e1, typeof' c e2) of
    (BitType _ _, BitType _ _) -> BoolType
    (_, _) -> error "Type error: comparison expected bits but did not get it"
typeof' c (BinaryOp _ e1 e2) =
  let (t1, t2) = (typeof' c e1, typeof' c e2)
   in intersect t1 t2
  where
    intersect (BitType lb1 ub1) (BitType lb2 ub2) =
      if lb2 > ub1 || lb1 > ub2
        then error "Type error: incompatible types in binary operation"
        else BitType (max lb1 lb2) (min ub1 ub2)
    intersect _ _ = error "Type error: incompatible types in binary operation"
{-
  In the let expression the user types in a size they want for the value. the
  actual value being bound may be some range, just need that exact size to be
  in the range to be okay
-}
typeof' c (Let id (BitTypeSyntax n) e1 e2) = typeof' (extend c id t') e2
  where
    t' =
      case typeof' c e1 of
        BitType lb ub ->
          if lb <= n && n <= ub
            then BitType n n
            else error "Type error: incompatible bit type with let expression"
        _ -> error "Type error: let binding expected bits but did not get it"
typeof' c (Let id BoolTypeSyntax e1 e2) = typeof' (extend c id t') e2
  where
    t' =
      case typeof' c e1 of
        BoolType -> BoolType
        _ -> error "Type error: let binding expected bool but did not get it"
typeof' c (Var id) =
  case lookup id c of
    Just x -> x
    Nothing -> error "Lookup failed"
typeof' c (If e1 e2 e3) =
  case typeof' c e1 of
    BoolType ->
      case (typeof' c e2, typeof' c e3) of
        (BoolType, BoolType) -> BoolType
        (t1, t2) -> intersect t1 t2
    _ -> error "Type error: not a boolean expression in the if expression"
  where
    intersect (BitType lb1 ub1) (BitType lb2 ub2) =
      if lb2 > ub1 || lb1 > ub2
        then error "Type error: disjoint bit types in if expression"
        else BitType (max lb1 lb2) (min ub1 ub2)
    intersect _ _ = error "Type error: different types in if expression"

{-
  evaluation rules:

  need to know the types while doing the evaluation. might be more useful to
  provide an upper bound on the size of types...

  on further thought, internally handle things as the largest possible width in
  the type. This would require a finite bound on the size of the types (which
  is reasonable, I think)

  if the type of the top-level expression could be some range of bits, we
  select the smallest width in the range (lower bound)

-}
data Value
  = ValBits Word Word -- width, val
  | ValBool Bool
  deriving (Show)

type Env = [(Id, Value)]

eval :: Expr -> Value
eval = eval' []

eval' :: Env -> Expr -> Value
eval' _ (LitInt n)
  | n > 0 =
    ValBits (ceiling $ logBase 2 $ (fromIntegral n) + 1) (fromIntegral n)
  | n == 0 = ValBits 1 0
  | n < 0 = ValBits w ((fromIntegral n) `mod` (2 ^ w))
  where
    w = max 1 $ ceiling $ logBase 2 $ fromIntegral $ abs n
eval' _ (LitBool b) = ValBool b
eval' r (UnaryOp Shrink e) = eval' r e
eval' r (UnaryOp Extend e) =
  let ValBits _ v = eval' r e
   in ValBits maxBits v
{-
  sign extension is tricky: if we don't know the exact size of the type, then
  we don't know what bit to look at for signedness (?)

  I'm thinking maybe use the lower bound... but using the type rules the value
  of
      sign-extend (shrink 5)
  would be `0`. Is that okay?

  But then that means we need to keep type information around to eval!!
-}
eval' r (UnaryOp SignExtend e) =
  let ValBits b v = eval' r e
   in if testBit v ((fromIntegral b) - 1)
      {-
        subtraction should be fine, since an invariant we maintain is that
        the width of all values least 1 (so we can get at least the 0th bit)
      -}
        then ValBits maxBits (signExtend b .|. v)
        else ValBits maxBits v
  where
    signExtend = \b -> foldr (.|.) 0 [bit (fromIntegral i) | i <- [b .. 31]]
eval' r (UnaryOp BoolNot e) =
  let ValBool b = eval' r e
   in ValBool $ not b
eval' r (BinaryOp (BoolOp op) e1 e2) =
  let (ValBool b1, ValBool b2) = (eval' r e1, eval' r e2)
   in ValBool $ op' b1 b2
  where
    op' =
      case op of
        BoolAnd -> (&&)
        BoolOr -> (||)
eval' r (BinaryOp (CompOp op) e1 e2) =
  let (ValBits _ v1, ValBits _ v2) = (eval' r e1, eval' r e2)
   in ValBool $ op' v1 v2 -- there is an important assumption here!
  where
    op' =
      case op of
        CompEqual -> (==)
eval' r (BinaryOp op e1 e2) =
  let (ValBits w1 v1, ValBits w2 v2) = (eval' r e1, eval' r e2)
   in let w = min w1 w2 -- skeptical about the use of min here
       in ValBits w $ (op' v1 v2) `mod` (2 ^ w)
  where
    op' =
      case op of
        ArithOp ArithPlus -> (+)
        BitOp BitAnd -> (.&.)
        BitOp BitOr -> (.|.)
        BitOp BitEor -> xor
-- lets and vars
eval' r (Let id (BitTypeSyntax w) e1 e2) =
  let (ValBits _ v) = eval' r e1
   in let v' = v `mod` (2 ^ w)
       in eval' ((id, (ValBits w v')) : r) e2
eval' r (Var id) =
  case lookup id r of
    Just v -> v
    Nothing -> error "eval: Lookup failed"
-- if
eval' r (If e1 e2 e3) =
  let ValBool b = eval' r e1 in
    if b
      then eval' r e2
      else eval' r e3

examples :: [Expr]
examples =
  [ BinaryOp
      (BitOp BitOr)
      (UnaryOp Extend (LitInt 5))
      (UnaryOp Shrink (LitInt 50))
  , Let "x" (BitTypeSyntax 3) (UnaryOp Shrink (LitInt 100)) (Var "x")
  , Let "x" (BitTypeSyntax 3) (UnaryOp Extend (LitInt (-1))) (Var "x")
  , Let "x" (BitTypeSyntax 3) (UnaryOp SignExtend (LitInt (-1))) (Var "x")
  , BinaryOp
      (ArithOp ArithPlus)
      (UnaryOp Extend (LitInt 5))
      (UnaryOp Extend (LitInt 5))
  , BinaryOp (ArithOp ArithPlus) (LitInt 5) (UnaryOp Extend (LitInt 5))
  , BinaryOp (ArithOp ArithPlus) (LitInt 5) (LitInt 5)
  , LitBool False
  , UnaryOp BoolNot (LitBool False)
  , If (LitBool True) (LitBool True) (LitBool False)
  , If (LitBool True) (LitInt 5) (LitInt 6)
  , If (LitBool True) (LitInt 25) (UnaryOp Extend (LitInt 3))
  , If
      (BinaryOp (CompOp CompEqual) (LitInt 5) (LitInt 6))
      (LitInt 25)
      (UnaryOp Extend (LitInt 3))
  ]
