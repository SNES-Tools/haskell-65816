{-
  This is a typed implementation of the basic arithmetic language, except that
  there is only one type. Therefore, we expect every program to type-check
  correctly!
-}

{-
  Syntax:

  e ::=                       expressions:
        n                     literal integer
        e + e                 addition
        e - e                 subtraction
        let x: t = e in e     let binding
        x                     variable

  v ::=                       values:
        n                     integer

  t ::=                       types:
        int                   integer

  G ::=                       contexts:     (the G stands for Gamma)
        0                     empty context
        G, x:t                term variable binding

-}

data Expr -- no info here, you're on your own when it fails :)
  = Lit Int
  | BinOp Op Expr Expr
  | Let Id Type Expr Expr
  | Var Id
  deriving (Show)

data Op = Plus | Minus deriving (Show)

type Id = String

{-
  Typing rules:

  t ::= int

  ---------- lit int
  |- n : int

  The age-old question: what is the type of (+)?
  This approach is simple and makes me think of ocaml

   G |- e1 : int    G |- e2 : int
  -------------------------------- add
        G |- e1 + e2 : int

   G |- e1 : int    G |- e2 : int
  -------------------------------- sub
        G |- e1 - e2 : int

   G |- e1 : t1     G, x : t1 |- e2 : t2
  --------------------------------------- let
      G |- let x: t1 = e1 in e2 : t2

   x : t in G
  ------------ lookup
   G |- x : t
-}

type Context = [(Id, Type)]

data Type = IntType deriving (Eq, Show) -- the only type!!

emptyContext :: Context
emptyContext = []

addBinding :: Context -> Id -> Type -> Context
addBinding ctx x bind = (x, bind) : ctx

typeof :: Expr -> Type
typeof = typeofCtx emptyContext

typeofCtx :: Context -> Expr -> Type -- there is only one implementation
typeofCtx _ (Lit _) = IntType
typeofCtx c (BinOp _ e1 e2) =
  if (typeofCtx c e1 == IntType) && (typeofCtx c e2 == IntType)
    then IntType
    else error "Type error: binary operation" -- helpful!
typeofCtx c (Let x t e1 e2) =
  if typeofCtx c e1 == t
    then typeofCtx (addBinding c x t) e2
    else error "Type error: variable binding"
typeofCtx c (Var x) = case lookup x c of
                        Just t -> t
                        Nothing -> error "Type error: variable lookup"

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

type Value = Int

eval :: Expr -> Value
eval = evalEnv []

type Env = [(Id, Value)]

evalEnv :: Env -> Expr -> Value
evalEnv  _ (Lit i) = i
-- relying on Haskell's `+` and `-`
evalEnv r (BinOp Plus e1 e2)  = (evalEnv r e1) + (evalEnv r e2)
evalEnv r (BinOp Minus e1 e2) = (evalEnv r e1) - (evalEnv r e2)
evalEnv r (Let x _ e b) = evalEnv ((x, evalEnv r e) : r) b
evalEnv r (Var x) = case lookup x r of
                      Just i -> i
                      Nothing -> error ("Variable not in scope: " ++ x)

example :: Expr
example =
  Let
    "x"
    IntType
    (BinOp Plus (Lit 42) (Let "y" IntType (Lit 84) (Var "y")))
    (BinOp Minus (BinOp Minus (Lit 50) (Lit 40)) (Var "x"))
