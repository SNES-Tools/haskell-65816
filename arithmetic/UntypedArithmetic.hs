{-
  This is an untyped implementation of arithmetic starting from Chapter 4 in
  TAPL... maybe also leveraging my 430 experience a lot too
-}

{-
  Syntax:

  e ::= n  -- literal integer
        e + e
        e - e
        let x = e in e
        x  -- variable
-}
data Expr -- no info here, you're on your own when it fails :)
  = Lit Int
  | BinOp Op Expr Expr
  | Let Id Expr Expr
  | Var Id
  deriving (Show)

data Op = Plus | Minus deriving (Show)

type Id = String

type Value = Int

type Env = [(Id, Value)]

{-
  evaluation rules (this is really from 330):

  --------- lit int
  A; n => n

  A; e1 => n1   A; e2 => n2   n3 = n1 + n2
  ---------------------------------------- add
             A; e1 + e2 => n3

  A; e1 => n1   A; e2 => n2   n3 = n1 - n2
  ---------------------------------------- sub
             A; e1 - e2 => n3

  A; e1 => v1    A,x:v1; v2
  ------------------------- let
  A; let x = e1 in e2 => v2

   A(x) = v
  ----------- lookup
   A; x => v
-}

eval :: Expr -> Value
eval = evalEnv []

evalEnv :: Env -> Expr -> Value
evalEnv  _ (Lit i) = i
-- relying on Haskell's `+` and `-`
evalEnv r (BinOp Plus e1 e2)  = (evalEnv r e1) + (evalEnv r e2)
evalEnv r (BinOp Minus e1 e2) = (evalEnv r e1) - (evalEnv r e2)
evalEnv r (Let x e b) = evalEnv ((x, evalEnv r e) : r) b
evalEnv r (Var x) = case lookup x r of
                      Just i -> i
                      Nothing -> error ("Variable not in scope: " ++ x)

example :: Expr
example =
  Let
    "x"
    (BinOp Plus (Lit 42) (Lit 42))
    (BinOp Minus (BinOp Minus (Lit 50) (Lit 40)) (Var "x"))
